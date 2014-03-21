{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Network.HDNS.MessageRender where

import Blaze.ByteString.Builder
import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString.Char8 as BS 
import qualified Data.ByteString.Lazy as BL (ByteString)
import Network.HDNS.Util (intToWord)
import Network.HDNS.Internal
import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Bits
import Data.Word
import Data.Monoid
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM (insert, lookup, empty)
import qualified Data.Map as M
import qualified Data.Attoparsec.Types as T (Parser)
import Data.Attoparsec (anyWord8, take)
import qualified Data.Attoparsec.ByteString.Lazy as AL
import Prelude hiding (lookup, take)
import Data.Maybe (fromMaybe)
----------------------------------------------------------------
type Render = State DomainOffsets Write
data DomainOffsets = DomainOffsets{
     domainToPos :: M.Map Domain Int
  ,  writePos :: Int
 }

instance Monoid Render where
    mempty = return mempty
    mappend a b = mconcat <$> sequence [a, b]

moveWritePos :: Int -> State DomainOffsets ()
moveWritePos n = do
    (DomainOffsets offsets writePos) <- get
    put $ DomainOffsets offsets (writePos + n)

writeUint8 :: Word8 -> Render
writeUint8 n = do
    moveWritePos 1
    return $ writeWord8 n

writeUint16 :: Word16 -> Render
writeUint16 n = do
    moveWritePos 2
    return $ writeWord16be n

writeUint32 :: Word32 -> Render
writeUint32 n = do
    moveWritePos 4
    return $ writeWord32be n

writeData :: ByteString -> Render
writeData d = do
    moveWritePos $ BS.length d
    return $ writeByteString d

writeDomain :: Domain -> Render
writeDomain name 
    | isRoot name = writeUint8 0
    | otherwise = do
        (DomainOffsets offsets writePos) <- get
        case M.lookup name offsets of
            Just pos -> writeUint16 . fromIntegral . toInteger $ (pos .|. 0xc000)
            Nothing -> (put $ DomainOffsets (M.insert name writePos offsets) writePos) >>
                writeLabel firstLabel <> writeDomain parentDomain 
        where 
            Just (firstLabel, parentDomain) = popLabel name
            

writeLabel :: Domain -> Render
writeLabel label = (writeUint8 . intToWord $ domainLen label)
                   <> writeData (rawName label)

rend :: Render -> BL.ByteString
rend = toLazyByteString . fromWrite . flip evalState initRenderState
        where initRenderState = DomainOffsets M.empty 0

----------------------------------------------------------------
type Parser = StateT OffsetToDomain (T.Parser ByteString)
data OffsetToDomain = OffsetToDomain {
    posToDomain :: IntMap Domain
  , readPos:: Int
 }

moveReadPos :: Int -> OffsetToDomain -> OffsetToDomain
moveReadPos n (OffsetToDomain offsets pos) = OffsetToDomain offsets (pos + n)

recordDomain :: Domain -> Int -> OffsetToDomain -> OffsetToDomain
recordDomain name offset (OffsetToDomain offsets pos) = 
    OffsetToDomain (IM.insert offset name offsets) pos

fetchDomain :: Int -> OffsetToDomain -> Maybe Domain
fetchDomain offset (OffsetToDomain offsets pos) = IM.lookup offset offsets

readUint8 :: Parser Word8
readUint8 = do
    modify $ moveReadPos 1
    lift anyWord8

readUint16 :: Parser Word16
readUint16 = do
    a <- fromIntegral <$> readUint8
    b <- fromIntegral <$> readUint8
    return $ (shiftL a 8) + b 

readUint32 :: Parser Word32
readUint32 = do
    a <- fromIntegral <$> readUint16
    b <- fromIntegral <$> readUint16
    return $ (shiftL a 16) + b 

readData :: Int -> Parser ByteString
readData len = do
    modify $ moveReadPos len
    lift (take len) 

readUint8Array :: Int -> Parser [Int]
readUint8Array n = toInts <$> (readData n)
    where 
        toInts :: ByteString -> [Int]
        toInts bs = map fromIntegral $ unpack bs

readDomain :: Parser Domain
readDomain = do
    offsets@(OffsetToDomain _ currentPos) <- get
    labelLen <- readUint8
    if labelLen == 0 then
        return rootDomain
    else do
        let n = normalize labelLen
        if isPointer labelLen then do
            o <- readUint8
            let offset = fromInteger $ n * 256 + (toInteger o)
            fromMaybe (error $ "decodeDomain error:" ++ show offset) <$> (return $ fetchDomain offset offsets)
        else do
            label <- readData . fromIntegral $ n
            parentDomain <- readDomain
            let domain = (Domain label) +++ parentDomain
            modify $ recordDomain domain currentPos
            return domain
    where 
        normalize c = toInteger $ c .&. 0x3f 
        isPointer c = testBit c 7 && testBit c 6


parse :: Parser a -> BL.ByteString -> Either String (a, OffsetToDomain)
parse parser bs = AL.eitherResult $ AL.parse (runStateT parser initParserState) bs
        where initParserState = OffsetToDomain IM.empty 0


--------------------------------------------------------------
--class ProtocolField a where
-----   decode      :: Parser a
--   encode      :: a -> Render 
--   toString    :: a -> String
--   fromString  :: String -> a
--
--ewtype (ProtocolField a) => ProtocolElem a = PE {e :: a}
--
--nstance ProtocolField Word8 where
--   decode = readUint8
--   encode = writeUint8
--   toString = show
--   fromString = read
--
--nstance ProtocolField Word16 where
--   decode = readUint16
--   encode = writeUint16
--   toString = show
--   fromString = read
--
--nstance ProtocolField Word32 where
--   decode = readUint32
--   encode = writeUint32
--   toString = show
--   fromString = read
--
--nstance ProtocolField Domain where
--   decode = readDomain
--   encode = writeDomain
--   toString = BS.unpack
----   fromString = BS.pack
