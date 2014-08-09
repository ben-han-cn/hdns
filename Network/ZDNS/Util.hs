{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-} 
module Network.ZDNS.Util where

import Data.Tuple (swap)
import Data.Maybe (fromJust, fromMaybe)
import Data.Bits
import System.Random (getStdRandom, randomR)
import Data.Word
import Data.Bits (shiftL)
import Data.List (intersperse)

-- | A mapper from type to Integeral(Int, Word)
class (Eq c, Integral i)  => CodeMapper c i where
    getMapper :: [(c, i)]
    unknownCode :: i -> c
    toWord :: c -> i

    fromWord :: i -> c
    fromWord n = 
        fromMaybe (unknownCode n) $ lookup n (swapMapper getMapper)
        where swapMapper m = map swap $ m

    knownCodeToWord:: c -> i
    knownCodeToWord t = fromJust $ lookup t getMapper


intToWord :: (Integral a) => Int -> a
intToWord = fromInteger . toInteger 
{-# INLINE intToWord #-}


data FlagDescriptor = FlagDescriptor Int Int
allOne :: (Bits a, Integral a) => Int -> a
allOne w = (1 `shiftL` w) - 1

flagMask :: (Bits a, Integral a) => FlagDescriptor -> a
flagMask (FlagDescriptor w p) = 
    (allOne w) `shiftL` p

getFlag :: (Bits a, Integral a) => a -> FlagDescriptor -> Int
getFlag flag fd@(FlagDescriptor _ p) = 
    fromIntegral $ (flag .&. (flagMask fd)) `shiftR` p

setFlag :: (Bits a, Integral a) => a -> FlagDescriptor -> Int -> a
setFlag flag fd@(FlagDescriptor _ p) val = 
    flag .|. valInFlag
    where valInFlag = ((intToWord val) `shiftL` p) .&. (flagMask fd) 

rand :: Int -> IO Int
rand m = randRange 0 m

randRange :: Int -> Int -> IO Int
randRange l u = getStdRandom (randomR (l, u))

ntohs :: [Word8] -> Word16
ntohs bs = let (a:b:[]) = bs
               in (shiftL (fromIntegral a) 8) + (fromIntegral b)

ntohl :: [Word8] -> Word32
ntohl bs = let (a:b:c:d:[]) = bs
               ab = ntohs [a,b]
               cd = ntohs [c,d]
               in (shiftL (fromIntegral ab) 16) + (fromIntegral cd)

join :: String -> [String] -> String
join sep strs = concat $ intersperse sep strs
