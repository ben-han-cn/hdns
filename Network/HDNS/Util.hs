{-# LANGUAGE RecordWildCards #-} 
module Network.HDNS.Util where
import Data.Tuple (swap)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.Word

class StringLike a where
    fromString :: String -> Maybe a
    toString :: a -> String

class (Eq a)  => CodeMapper a where
    getMapper :: [(a, Word16)]
    unknowCode :: Word16 -> a

    fromWord16 :: Word16 -> a
    fromWord16 n = fromMaybe (unknowCode n) $ lookup n (swapMapper getMapper)
        where swapMapper m = map swap $ m

    toWord16 :: a -> Word16
    toWord16 t = fromMaybe 0 $ lookup t getMapper


intToWord :: (Integral a) => Int -> a
intToWord = fromInteger . toInteger 
