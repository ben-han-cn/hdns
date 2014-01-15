{-# LANGUAGE OverloadedStrings #-}

module Network.HDNS.Decode (
    decode
  ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (unpack)
import Data.IP
import Data.Maybe
import Data.Word
import Network.HDNS.Util (fromWord16)
import Network.HDNS.Internal
import Network.HDNS.MessageRender
----------------------------------------------------------------

-- | Parsing DNS data.

decode :: BL.ByteString -> Either String DNSFormat
decode bs = fst <$> parse parseResponse bs

----------------------------------------------------------------

parseResponse :: Parser DNSFormat
parseResponse = do
    hd <- parseHeader 
    DNSFormat hd <$> parseQueries (qdCount hd)
                 <*> parseRRs (anCount hd)
                 <*> parseRRs (nsCount hd)
                 <*> parseRRs (arCount hd)

----------------------------------------------------------------

parseFlags :: Parser DNSFlags
parseFlags = toFlags <$> readUint16
  where
    toFlags flgs = DNSFlags (getQorR flgs)
                            (getOpcode flgs)
                            (getAuthAnswer flgs)
                            (getTrunCation flgs)
                            (getRecDesired flgs)
                            (getRecAvailable flgs)
                            (getRcode flgs)
    getQorR w = if testBit w 15 then QR_Response else QR_Query
    getOpcode w = toEnum $ fromIntegral $ shiftR w 11 .&. 0x0f
    getAuthAnswer w = testBit w 10
    getTrunCation w = testBit w 9
    getRecDesired w = testBit w 8
    getRecAvailable w = testBit w 7
    getRcode w = toEnum $ fromIntegral $ w .&. 0x0f

----------------------------------------------------------------

parseHeader :: Parser DNSHeader
parseHeader = DNSHeader <$> readUint16
                         <*> parseFlags
                         <*> readUint16
                         <*> readUint16
                         <*> readUint16
                         <*> readUint16
----------------------------------------------------------------

parseQueries :: Word16 -> Parser [Question]
parseQueries n = replicateM (fromIntegral n) parseQuery

parseType :: Parser TYPE
parseType = fromWord16 <$> readUint16

parseQuery :: Parser Question
parseQuery = Question <$> readDomain
                       <*> (fromWord16 <$> readUint16)
                       <*> (fromWord16 <$> readUint16)

parseRRs :: Word16 -> Parser [ResourceRecord]
parseRRs n = replicateM (fromIntegral n) parseRR 

parseRR :: Parser ResourceRecord
parseRR = do
    Question dom typ klass <- parseQuery 
    ttl <- readUint32
    len <- readUint16
    dat <- parseRData typ len
    return ResourceRecord { rrname = dom
                          , rrtype = typ
                          , rrklass = klass
                          , rrttl  = ttl
                          , rdlen  = len
                          , rdata  = dat
                          }
parseRData :: TYPE -> Word16 -> Parser RDATA
parseRData NS _ = RD_NS <$> readDomain
parseRData MX _ = RD_MX <$> readUint16 <*> readDomain
parseRData CNAME _ = RD_CNAME <$> readDomain
parseRData TXT len = (RD_TXT . ignoreLength) <$> readData (fromIntegral len)
  where
    ignoreLength = BS.tail
parseRData A len  = (RD_A . toIPv4 ) <$> readUint8Array (fromIntegral len)
parseRData AAAA len  = (RD_AAAA . toIPv6 . combine ) <$> readUint8Array (fromIntegral len)
  where
    combine [] = []
    combine [_] = error "combine"
    combine (a:b:cs) =  a * 256 + b : combine cs
parseRData SOA _ = RD_SOA <$> readDomain
                           <*> readDomain
                           <*> readUint32
                           <*> readUint32
                           <*> readUint32
                           <*> readUint32
                           <*> readUint32
parseRData PTR _ = RD_PTR <$> readDomain
parseRData SRV _ = RD_SRV <$> readUint16
                           <*> readUint16
                           <*> readUint16
                           <*> readDomain
parseRData _  len = RD_OTH <$> unpack <$> readData (fromIntegral len)
