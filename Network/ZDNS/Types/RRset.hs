{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module Network.ZDNS.Types.RRset where

import Data.Word
import Data.IP
import Data.Char (toUpper)
import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.List as L
import Network.ZDNS.Types.Name
import Network.ZDNS.Util
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16

----------------------------------------------------------------
data RRType = A | AAAA | NS | TXT | MX | CNAME | SOA | PTR | SRV | NAPTR 
              | OPT | DS | RRSIG | DNSKEY | NSEC3 | NSEC3PARAM
              | UNKNOWNTYPE Word16 deriving (Eq, Show, Read)

instance CodeMapper RRType Word16 where
    getMapper = [ 
                (A,              1)  
              , (NS,             2)  
              , (CNAME,          5)  
              , (SOA,            6)  
              , (PTR,           12) 
              , (MX,            15) 
              , (TXT,           16) 
              , (AAAA,          28) 
              , (SRV,           33) 
              , (NAPTR,         35) 
              , (OPT,           41) 
              , (DS,            43) 
              , (RRSIG,         46) 
              , (DNSKEY,        48) 
              , (NSEC3,         50) 
              , (NSEC3PARAM,    51) 
              ]   
    unknownCode = UNKNOWNTYPE

    toWord (UNKNOWNTYPE t) = t
    toWord ot = knownCodeToWord ot
----------------------------------------------------------------
data RRClass = IN | CS | CH | HS | UNKNOWNKLASS Word16 deriving (Eq, Show, Read)
instance CodeMapper RRClass Word16 where
    getMapper = [
                  (IN,  1)
                , (CS,  2)
                , (CH,  3)
                , (HS,  4)
                ]
    unknownCode = UNKNOWNKLASS
    toWord (UNKNOWNKLASS c) = c
    toWord oc = knownCodeToWord oc
----------------------------------------------------------------
data RdataFieldType = RCompressedDomain
                    | RUnCompressedDomain
                    | RBinary
                    | RByteBinary
                    | RString
                    | RTXT
                    | RByte
                    | RShort
                    | RLong
                    | RIPv4
                    | RIPv6 deriving (Eq)

data RdataField = RDFCompressedDomain !Domain
                | RDFUnCompressedDomain !Domain
                | RDFByte !Word8
                | RDFShort !Word16
                | RDFLong !Word32
                | RDFBinary !ByteString
                | RDFByteBinary !(Word8, ByteString)
                | RDFString !ByteString
                | RDFTXT !(V.Vector ByteString)
                | RDFIPv4 !IPv4
                | RDFIPv6 !IPv6 deriving (Eq, Ord)

type Rdata = V.Vector RdataField
showRdata :: Rdata -> RRType -> String
showRdata rdata DS = let (tag:alg:digest_type:(RDFBinary digtest):_) = (V.toList rdata)
                        in L.intercalate " " [show tag 
                                             ,show alg
                                             ,show digest_type
                                             ,L.map toUpper $ BC.unpack . B16.encode $ digtest] 
showRdata rdata RRSIG = let ((RDFShort ct):alg:labels:ttl:expire:inception:key_tag:signer:(RDFBinary sig):_) = (V.toList rdata)
                            ctype = (fromWord ct) :: RRType
                            in L.intercalate " " [show ctype
                                                 ,show alg
                                                 ,show labels
                                                 ,show ttl
                                                 ,show expire
                                                 ,show inception
                                                 ,show key_tag
                                                 ,show signer
                                                 ,L.map toUpper $ BC.unpack . B64.encode $ sig] 
showRdata rdata DNSKEY = let (flag:protocol:alg:(RDFBinary key):_) = (V.toList rdata)
                        in L.intercalate " " [show flag
                                             ,show protocol
                                             ,show alg
                                             ,L.map toUpper $ BC.unpack . B64.encode $ key] 

showRdata rdata NSEC3PARAM = let (ht:flags:iteration:(RDFByteBinary (_, salt)):_) = (V.toList rdata)
                        in L.intercalate " " [show ht
                                             ,show flags
                                             ,show iteration
                                             ,L.map toUpper $ BC.unpack . B16.encode $ salt] 
showRdata rdata _ = L.intercalate " " $ L.map show $ V.toList rdata

instance Show RdataField where
    show (RDFCompressedDomain n) = show n
    show (RDFUnCompressedDomain n) = show n
    show (RDFByte w) = show w
    show (RDFShort w) = show w
    show (RDFLong w) = show w
    show (RDFBinary b) = show b
    show (RDFByteBinary (_, b)) = show b
    show (RDFString s) = "\"" ++ show s ++ "\""
    show (RDFTXT b) = show b
    show (RDFIPv4 ip) = show ip
    show (RDFIPv6 ip) = show ip

type RdataVec = V.Vector Rdata
type TTL = Word32
data RRset = RRset {
    rrsetName   :: Domain
 ,  rrsetType   :: RRType
 ,  rrsetClass  :: RRClass
 ,  rrsetTTL    :: TTL
 ,  rrsetRdatas :: RdataVec
 }

addRdata :: Rdata -> RRset -> RRset
addRdata rdata (RRset n t c ttl rdatas) = 
    RRset n t c ttl (V.snoc rdatas rdata)

showRRset :: RRset -> String
showRRset (RRset n t c ttl rdatas) = 
    L.intercalate "\n" $ L.map showRR (V.toList rdatas)
    where showRR rdata = L.intercalate "\t" [show n
                                            ,show ttl
                                            ,show c
                                            ,show t
                                            ,showRdata rdata t]

instance Show RRset where
    show = showRRset

type RdataDescriptor = [RdataFieldType]

getRdataDescriptor :: RRType -> RdataDescriptor
getRdataDescriptor NS = [RCompressedDomain]
getRdataDescriptor CNAME = [RCompressedDomain]
getRdataDescriptor MX = [RShort, RCompressedDomain]
getRdataDescriptor PTR = [RCompressedDomain]
getRdataDescriptor SOA = [RCompressedDomain
                        , RCompressedDomain
                        , RLong
                        , RLong
                        , RLong
                        , RLong
                        , RLong]
getRdataDescriptor A = [RIPv4]
getRdataDescriptor AAAA = [RIPv6]
getRdataDescriptor TXT = [RTXT]
getRdataDescriptor SRV = [RShort, RShort, RShort, RCompressedDomain]
getRdataDescriptor NAPTR = [RShort, RShort, RString, RString, RString, RCompressedDomain]
getRdataDescriptor DS = [RShort, RByte, RByte, RBinary]
getRdataDescriptor RRSIG = [RShort, RByte, RByte, RLong, RLong, RLong, RShort, RCompressedDomain, RBinary]
getRdataDescriptor DNSKEY = [RShort, RByte, RByte, RBinary]
getRdataDescriptor NSEC3 = [RByte, RByte, RShort, RByteBinary, RByteBinary, RBinary]
getRdataDescriptor NSEC3PARAM = [RByte, RByte, RShort, RByteBinary]
getRdataDescriptor OPT = [RBinary]
getRdataDescriptor (UNKNOWNTYPE _) = [RBinary]
