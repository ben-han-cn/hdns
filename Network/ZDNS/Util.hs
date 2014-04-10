{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-} 
module Network.ZDNS.Util where

import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import Data.Bits

-- | A mapper from type to Integeral(Int, Word)
class (Eq c, Integral i)  => CodeMapper c i where
    getMapper :: [(c, i)]
    unknowCode :: i -> c

    fromWord :: i -> c
    fromWord n = 
        fromMaybe (unknowCode n) $ lookup n (swapMapper getMapper)
        where swapMapper m = map swap $ m

    toWord :: c -> i
    toWord t = fromMaybe 0 $ lookup t getMapper


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
