{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module Network.ZDNS.Types.RRset where

import Data.Word
import Data.IP
import Data.ByteString
import qualified Data.Vector as V
import qualified Data.List as L
import Network.ZDNS.Types.Name
import Network.ZDNS.Util

----------------------------------------------------------------
data RRType = A | AAAA | NS | TXT | MX | CNAME | SOA | PTR | SRV | NAPTR
              | UNKNOWNTYPE Word16 deriving (Eq, Show, Read)

instance CodeMapper RRType Word16 where
    getMapper = [ 
                (A,      1)  
              , (NS,     2)  
              , (CNAME,  5)  
              , (SOA,    6)  
              , (PTR,   12) 
              , (MX,    15) 
              , (TXT,   16) 
              , (AAAA,  28) 
              , (SRV,   33) 
              , (NAPTR,   35) 
              ]   
    unknowCode = UNKNOWNTYPE
----------------------------------------------------------------
data RRClass = IN | CS | CH | HS | UNKNOWNKLASS Word16 deriving (Eq, Show, Read)
instance CodeMapper RRClass Word16 where
    getMapper = [
                  (IN,  1)
                , (CS,  2)
                , (CH,  3)
                , (HS,  4)
                ]
    unknowCode = UNKNOWNKLASS
----------------------------------------------------------------
data RdataFieldType = RCompressedDomain
                    | RUnCompressedDomain
                    | RBinary
                    | RString
                    | RTXT
                    | RByte
                    | RShort
                    | RLong
                    | RIPv4
                    | RIPv6 deriving (Eq)

data RdataField = RDFCompressedDomain Domain
                | RDFUnCompressedDomain Domain
                | RDFByte Word8
                | RDFShort Word16
                | RDFLong Word32
                | RDFBinary ByteString
                | RDFString ByteString
                | RDFTXT (V.Vector ByteString)
                | RDFIPv4 IPv4
                | RDFIPv6 IPv6 deriving (Eq, Ord)

type Rdata = V.Vector RdataField
showRdata :: Rdata -> String
showRdata rdata = L.intercalate " " $ L.map show $ V.toList rdata

instance Show RdataField where
    show (RDFCompressedDomain n) = show n
    show (RDFUnCompressedDomain n) = show n
    show (RDFByte w) = show w
    show (RDFShort w) = show w
    show (RDFLong w) = show w
    show (RDFBinary b) = show b
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
                                            ,show t
                                            ,show c
                                            ,show ttl
                                            ,showRdata rdata]

instance Show RRset where
    show = showRRset

type RdataDescriptor = [RdataFieldType]

getRdataDescriptor :: RRType -> RdataDescriptor
getRdataDescriptor NS = [RCompressedDomain]
getRdataDescriptor CNAME = [RCompressedDomain]
getRdataDescriptor MX = [RCompressedDomain]
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
getRdataDescriptor (UNKNOWNTYPE _) = [RBinary]
