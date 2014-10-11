{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings, RecordWildCards #-}
module Network.ZDNS.MessageRender where

import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString.Char8 as BS 
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Blaze.ByteString.Builder as BB 
import Network.ZDNS.Types.Name
import Network.ZDNS.Types.RRset
import Network.ZDNS.Types.Message
import Network.ZDNS.Util
import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
import Data.Bits
import Data.Word
import Data.Monoid
import Data.IP
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM (insert, lookup, empty)
import qualified Data.Map as M
import qualified Data.Attoparsec.Types as T (Parser)
import Data.Attoparsec (anyWord8, take)
import qualified Data.Attoparsec.ByteString.Lazy as AL
import Prelude hiding (lookup, take)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Vector as V
----------------------------------------------------------------
type Render = State DomainOffsets BB.Write
data DomainOffsets = DomainOffsets{
     domainToPos :: M.Map Domain Int
  ,  writePos :: Int
 }

-- |BB.Write is a monoid
instance Monoid Render where
    mempty = return mempty
    mappend a b = mconcat <$> sequence [a, b]

moveWritePos :: Int -> State DomainOffsets ()
moveWritePos n = do
    (DomainOffsets offsets p) <- get
    put $ DomainOffsets offsets (p + n)

writeUint8 :: Word8 -> Render
writeUint8 n = do
    moveWritePos 1
    return $ BB.writeWord8 n

writeUint16 :: Word16 -> Render
writeUint16 n = do
    moveWritePos 2
    return $ BB.writeWord16be n

writeUint32 :: Word32 -> Render
writeUint32 n = do
    moveWritePos 4
    return $ BB.writeWord32be n

writeData :: ByteString -> Render
writeData d = do
    moveWritePos $ BS.length d
    return $ BB.writeByteString d

writeLabel :: Label -> Render
writeLabel l@(Label d1 l1) =
    if l == emptyLabel 
    then writeUint8 0
    else (writeUint8 $ intToWord l1) <> (writeData d1) 

writeDomain :: Domain -> Render
writeDomain name@(Domain ls _) 
    | V.length ls == 1 = writeLabel $ V.head ls
    | otherwise =  do
        (DomainOffsets offsets p) <- get
        case M.lookup name offsets of
            Just pos -> writeUint16 . fromIntegral . toInteger $ (pos .|. 0xc000)
            Nothing ->
                (put $ DomainOffsets (M.insert name p offsets) p) >>
                    writeLabel (V.head ls) <> (writeDomain $ fromJust $ superDomain 1 name)

writeDomainNOC :: Domain -> Render
writeDomainNOC name@(Domain ls _) 
    | V.length ls == 1 = writeLabel $ V.head ls
    | otherwise =  do
        (DomainOffsets offsets p) <- get
        (put $ DomainOffsets (M.insert name p offsets) p) >>
            writeLabel (V.head ls) <> (writeDomainNOC $ fromJust $ superDomain 1 name)

writeMessage :: Message -> Render
writeMessage Message{..} = writeHeader header
                        <> writeQuestion question
                        <> writeSection answer
                        <> writeSection authority
                        <> writeSection additional 
                        where writeSection s = mconcat (map writeRRset $ V.toList s)

writeHeader :: Header -> Render
writeHeader Header{..} = writeUint16 hid
                      <> writeUint16 flags
                      <> writeUint16 qcount
                      <> writeUint16 anscount
                      <> writeUint16 authcount
                      <> writeUint16 addicount

writeRRset :: RRset -> Render
writeRRset RRset{..} = 
    mconcat (map writeRR $ V.toList rrsetRdatas)
    where writeRR rdata = writeDomain rrsetName 
                       <> (writeUint16 . toWord $ rrsetType)
                       <> (writeUint16 . toWord $ rrsetClass)
                       <> writeUint32 rrsetTTL
                       <> writeRdatasAndLen rdata
          writeRdatasAndLen rdata = do
            moveWritePos 2
            rdataWire <- writeRdata rdata
            let rdataLength = fromIntegral . BS.length . BB.toByteString . BB.fromWrite $ rdataWire
            moveWritePos (-2)
            (writeUint16 rdataLength) <> (return rdataWire)

writeRdata :: Rdata -> Render
writeRdata rdata = mconcat (map writeRdataField $ V.toList rdata)

writeRdataField :: RdataField -> Render
writeRdataField (RDFCompressedDomain d) = writeDomain d
writeRdataField (RDFUnCompressedDomain d) = writeDomainNOC d
writeRdataField (RDFByte d) = writeUint8 d
writeRdataField (RDFShort d) = writeUint16 d
writeRdataField (RDFTXT txts) = mconcat (map writeData $ V.toList txts)
writeRdataField (RDFLong d) = writeUint32 d
writeRdataField (RDFIPv4 ip) = mconcat $ map (writeUint8 . intToWord)  (fromIPv4 ip)
writeRdataField (RDFIPv6 ip) = mconcat $ map (writeUint16 . intToWord)  (fromIPv6 ip)
writeRdataField (RDFBinary d) = writeData d
writeRdataField (RDFByteBinary (len, d)) = (writeUint8 len) <> (writeData d)
writeRdataField (RDFString d) = writeData d
        
writeQuestion :: Question -> Render
writeQuestion Question{..} = writeDomain qname 
                          <> (writeUint16 . toWord $ qtype)
                          <> (writeUint16 . toWord $ qclass)
                                                                    
rend :: Render -> BL.ByteString
rend = BB.toLazyByteString . BB.fromWrite . flip evalState initRenderState
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
fetchDomain offset (OffsetToDomain offsets _) = IM.lookup offset offsets


readMessage :: Parser Message
readMessage = do
    h <- readHeader
    q <- readQuestion
    ans <- readSection $ anscount h
    auth <- readSection $ authcount h
    addi <- readSection $ addicount h
    return $ Message h q ans auth addi

readHeader :: Parser Header
readHeader = do
    Header <$> readUint16 
           <*> readUint16
           <*> readUint16
           <*> readUint16
           <*> readUint16
           <*> readUint16

readSection :: Word16 -> Parser Section
readSection n = foldl (\s rs -> addRRToSection rs s) V.empty <$> replicateM (fromIntegral n) readRRset 

readQuestion :: Parser Question
readQuestion = do
    Question <$> readDomain 
             <*> (fromWord <$> readUint16)
             <*> (fromWord <$> readUint16)

readRRset :: Parser RRset
readRRset = do
    name <- readDomain
    rrtype <- (fromWord <$> readUint16)
    rrclass <- (fromWord <$> readUint16)
    ttl <- readUint32
    rdata <- readRdata rrtype
    return $ RRset name rrtype rrclass ttl (V.fromList [rdata])

readInLen :: Word16 -> (Word16 -> Parser a) -> Parser (Maybe a, Word16)
readInLen len f = do
    (OffsetToDomain _ posBeforeParse) <- get
    a <- f len
    (OffsetToDomain _ currentPos) <- get
    let consumeLen = fromIntegral $ currentPos - posBeforeParse 
    if consumeLen > len
    then return (Nothing, consumeLen)
    else return (Just a, len - consumeLen)
 
readRdata :: RRType -> Parser Rdata
readRdata rrtype = do 
    len <- readUint16
    readRdataHelper len (getRdataDescriptor rrtype) V.empty

readRdataHelper :: Word16 -> [RdataFieldType] -> Rdata -> Parser Rdata
readRdataHelper _ [] readFields = return readFields
readRdataHelper len (x:xs) readFields = do
        (result, leftLen) <- readInLen len (readRdataField x)
        case result of 
            Nothing -> return $ error "parse rdata failed"
            Just f -> readRdataHelper leftLen xs $ V.snoc readFields f

readRdataField :: RdataFieldType -> Word16 -> Parser RdataField
readRdataField RCompressedDomain _ = RDFCompressedDomain <$> readDomain
readRdataField RUnCompressedDomain _ = RDFUnCompressedDomain <$> readDomain
readRdataField RByte _ = RDFByte <$> readUint8 
readRdataField RShort _ = RDFShort <$> readUint16
readRdataField RTXT len = RDFTXT <$> readTXTLabels len V.empty
readRdataField RLong _ = RDFLong <$> readUint32
readRdataField RIPv4 _ = (RDFIPv4 . toIPv4) <$> readArray 4 readUint8 
readRdataField RIPv6 _ = (RDFIPv6 . toIPv6) <$> readArray 8 readUint16
readRdataField RBinary len = RDFBinary <$> (readData $ fromIntegral len)
readRdataField RByteBinary _ = do
                                ll <- readUint8
                                d <- readData $ fromIntegral ll
                                return $ RDFByteBinary (ll, d)
readRdataField RString _ = RDFString . fst <$> readLabel
        
readLabel :: Parser (ByteString, Word16)
readLabel = do
    ll <- fromIntegral <$> readUint8
    l <- readData $ fromIntegral ll
    return (l, ll)

readTXTLabels :: Word16 -> (V.Vector ByteString) -> Parser (V.Vector ByteString)
readTXTLabels len ls = do
    (l, ll) <- readLabel
    if len == ll + 1 -- 1 byte lable len itself
    then return $ V.snoc ls l
    else readTXTLabels (len - ll - 1) $ V.snoc ls l 

readDomain :: Parser Domain
readDomain = do
    offsets@(OffsetToDomain _ p) <- get
    ll <- readUint8
    if ll == 0 then
        return rootDomain
    else do
        let n = normalize ll
        if isPointer ll then do
            ll'  <- readUint8
            let offset = fromInteger $ n * 256 + (toInteger ll')
            fromMaybe (error $ "decodeDomain error:" ++ show offset) <$> (return $ fetchDomain offset offsets)
        else do
            ld <- readData . fromIntegral $ n
            case (mkLabel' ld) of
                Nothing -> return $ error "decode label failed"
                Just l -> do
                    parentDomain <- readDomain
                    case concatDomain (fromJust $ mkDomain' $ V.fromList [l, emptyLabel]) parentDomain of
                        Nothing -> return $ error "decode domain failed"
                        Just domain -> do
                            modify $ recordDomain domain p
                            return domain
    where 
        normalize c = toInteger $ c .&. 0x3f 
        isPointer c = testBit c 7 && testBit c 6

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

readArray :: Integral a => Int -> Parser a -> Parser [Int]
readArray count elem_reader = (map fromIntegral) <$> (replicateM count elem_reader)

parse :: Parser a -> BL.ByteString -> Either String (a, OffsetToDomain)
parse parser bs = AL.eitherResult $ AL.parse (runStateT parser initParserState) bs
        where initParserState = OffsetToDomain IM.empty 0
