{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeSynonymInstances #-}

module Network.HDNS.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.IP
import Data.Maybe
import Data.Word
import Data.List (intercalate, concat)
import Network.HDNS.Util

----------------------------------------------------------------

-- | Type for domain.
newtype Domain = Domain {
    rawName :: ByteString
} deriving (Eq, Ord)

parentDomain :: Domain -> Maybe Domain
parentDomain name = case popLabel name of
                        Just (_, parent) -> Just parent
                        Nothing -> Nothing
                    
instance StringLike Domain where
    toString = BS.unpack . rawName
    fromString str = Just (Domain $ BS.pack $ map toLower str)

instance Show Domain where
    show = toString

(+++) :: Domain -> Domain -> Domain
(+++) name1 name2  
    | isFQDN name1 = if isRoot name2 
                     then name1 
                     else concatDomains [name1, name2]
    | otherwise = if isRoot name2 
                  then concatDomains [name1, name2] 
                  else concatDomains [name1, rootDomain, name2]
    where 
        concatDomains domains = Domain $ BS.concat (map rawName domains)

isRoot :: Domain -> Bool
isRoot name = rawName name == rawName rootDomain

isFQDN :: Domain -> Bool
isFQDN name | isRoot name = True
            | (BS.last $ rawName name) == '.' = True
            | otherwise = False

qualifyDomain :: Domain -> Domain
qualifyDomain name 
    | isFQDN name = name
    | otherwise = name +++ rootDomain

rootDomain :: Domain
rootDomain = Domain $ BS.pack "."

domainLen :: Domain -> Int
domainLen = BS.length . rawName 

popLabel :: Domain -> Maybe (Domain, Domain)
popLabel name | isRoot name = Nothing
              | otherwise = 
                    let (label, parent) = BS.break (=='.') (rawName name)
                        parent' = if BS.null parent || parent == rawName rootDomain 
                                  then rootDomain 
                                  else Domain(BS.drop 1 parent)
                    in Just (Domain label, parent')
----------------------------------------------------------------

-- | Types for resource records.
data TYPE = A | AAAA | NS | TXT | MX | CNAME | SOA | PTR | SRV
          | UNKNOWNTYPE Word16 deriving (Eq, Show, Read)

instance CodeMapper TYPE where
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
              ]
    unknowCode = UNKNOWNTYPE

instance StringLike TYPE where
    fromString str = Just $ read (map toUpper str)
    toString = show
----------------------------------------------------------------
data Klass = IN | CS | CH | HS | UNKNOWNKLASS Word16 deriving (Eq, Show, Read)

instance CodeMapper Klass where
    getMapper = [
                  (IN,  1)
                , (CS,  2)
                , (CH,  3)
                , (HS,  4)
                ]
    unknowCode = UNKNOWNKLASS

instance StringLike Klass where
    fromString str = Just $ read (map toUpper str)
    toString = show
----------------------------------------------------------------

-- | An enumeration of all possible DNS errors that can occur.
data DNSError =
  -- | The sequence number of the answer doesn't match our query. This
  --   could indicate foul play.
  SequenceNumberMismatch
  -- | The request simply timed out.
  | TimeoutExpired
  -- | The answer has the correct sequence number, but returned an
  --   unexpected RDATA format.
  | UnexpectedRDATA
  deriving (Eq, Show)

-- | Raw data format for DNS Query and Response.
data DNSFormat = DNSFormat {
    header     :: DNSHeader
  , question   :: [Question]
  , answer     :: [ResourceRecord]
  , authority  :: [ResourceRecord]
  , additional :: [ResourceRecord]
  } deriving (Eq)
instance Show DNSFormat where
    show DNSFormat{..} = intercalate "\n" [(show header)
                                           , ";; QUESTION SECTION:"
                                           , (show $ head question)
                                           , ";; ANSWER SECTION:"
                                           , (showSection answer)
                                           , ";; AUTHORITY SECTION:"
                                           , (showSection authority)
                                           , ";; ADDITIONAL SECTION:"
                                           , (showSection additional)]

showSection :: [ResourceRecord] -> String
showSection rrs = concat $ map show rrs

-- | Raw data format for the header of DNS Query and Response.
data DNSHeader = DNSHeader {
    identifier :: Word16
  , flags      :: DNSFlags
  , qdCount    :: Word16
  , anCount    :: Word16
  , nsCount    :: Word16
  , arCount    :: Word16
  } deriving (Eq)


-- ;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 40542
-- ;; flags: qr rd ra; QUERY: 1, ANSWER: 3, AUTHORITY: 0, ADDITIONAL: 0
instance Show DNSHeader where
    show DNSHeader{..} = ";; ->>HEADER<<- " ++
                         "opcode: " ++ (show $ opcode flags) ++ "," ++
                         "status: " ++ (show $ rcode flags) ++ "," ++
                         "id: " ++ (show identifier) ++ "\n" ++
                         ";; flags: " ++ (show flags) ++ "; " ++ 
                         "QUERY: " ++ (show qdCount) ++ ", " ++
                         "ANSWER: " ++ (show anCount) ++ ", " ++
                         "AUTHORITY: " ++ (show nsCount) ++ ", " ++
                         "ADDITIONAL: " ++ (show arCount) ++ "\n"

-- | Raw data format for the flags of DNS Query and Response.
data DNSFlags = DNSFlags {
    qOrR         :: QorR
  , opcode       :: OPCODE
  , authAnswer   :: Bool
  , trunCation   :: Bool
  , recDesired   :: Bool
  , recAvailable :: Bool
  , rcode        :: RCODE
  } deriving (Eq)

instance Show DNSFlags where
    show DNSFlags {..} = intercalate " " (filter (not . null) 
                                          [show qOrR,
                                          showBoolFlag authAnswer "aa",
                                          showBoolFlag trunCation "tc",
                                          showBoolFlag recDesired "rd",
                                          showBoolFlag recAvailable "ra"])

showBoolFlag :: Bool -> String -> String
showBoolFlag True display = display
showBoolFlag False _ = ""
----------------------------------------------------------------

data QorR = QR_Query | QR_Response deriving (Eq)
instance Show QorR where
    show QR_Query = "qr"
    show _ = ""

data OPCODE = OP_STD | OP_INV | OP_SSR deriving (Eq, Enum)
instance Show OPCODE where
    show OP_STD = "QUERY"
    show _ = ""

data RCODE = NoErr | FormatErr | ServFail | NameErr | NotImpl | Refused deriving (Eq, Show, Enum)

----------------------------------------------------------------

-- | Raw data format for DNS questions.
data Question = Question {
    qname  :: Domain
  , qtype  :: TYPE
  , qklass :: Klass
  } deriving (Eq)

instance Show Question where
    show Question{..} = (intercalate "\t" [show qname, show qklass, show qtype]) ++ "\n"

-- | Making "Question".
makeQuestion :: Domain -> TYPE -> Klass -> Question
makeQuestion = Question

----------------------------------------------------------------

-- | Raw data format for resource records.
data ResourceRecord = ResourceRecord {
    rrname  :: Domain
  , rrtype  :: TYPE
  , rrklass :: Klass
  , rrttl   :: Word32
  , rdlen   :: Word16
  , rdata   :: RDATA
  } deriving (Eq)


instance Show ResourceRecord where
  show ResourceRecord{..} = intercalate "\t" [show rrname,
                              show rrtype,
                              show rrklass,
                              show rrttl,
                              show rdata,
                              "\n"]

-- | Raw data format for each type.
data RDATA = RD_NS Domain | RD_CNAME Domain | RD_MX Word16 Domain | RD_PTR Domain
           | RD_SOA Domain Domain Word32 Word32 Word32 Word32 Word32
           | RD_A IPv4 | RD_AAAA IPv6 | RD_TXT ByteString
           | RD_SRV Word16 Word16 Word16 Domain
           | RD_OTH [Word8] deriving (Eq)

instance Show RDATA where
  show (RD_NS dom) = show dom
  show (RD_MX prf dom) = show dom ++ " " ++ show prf
  show (RD_CNAME dom) = show dom
  show (RD_A a) = show a
  show (RD_AAAA aaaa) = show aaaa
  show (RD_TXT txt) = BS.unpack txt
  show (RD_SOA mn _ _ _ _ _ mi) = show mn ++ " " ++ show mi
  show (RD_PTR dom) = show dom
  show (RD_SRV pri wei prt dom) = show pri ++ " " ++ show wei ++ " " ++ show prt ++ show dom
  show (RD_OTH is) = show is

----------------------------------------------------------------

defaultQuery :: DNSFormat
defaultQuery = DNSFormat {
    header = DNSHeader {
       identifier = 0
     , flags = DNSFlags {
           qOrR         = QR_Query
         , opcode       = OP_STD
         , authAnswer   = False
         , trunCation   = False
         , recDesired   = True
         , recAvailable = False
         , rcode        = NoErr
         }
     , qdCount = 0
     , anCount = 0
     , nsCount = 0
     , arCount = 0
     }
  , question   = []
  , answer     = []
  , authority  = []
  , additional = []
  }

defaultResponse :: DNSFormat
defaultResponse =
  let hd = header defaultQuery
      flg = flags hd
  in  defaultQuery {
        header = hd {
          flags = flg {
              qOrR = QR_Response
            , authAnswer = True
            , recAvailable = True
            }
        }
      }

responseA :: Word16 -> Question -> IPv4 -> DNSFormat
responseA ident q ip =
  let hd = header defaultResponse
      dom = qname q
      an = ResourceRecord dom A IN 300 4 (RD_A ip)
  in  defaultResponse {
          header = hd { identifier=ident, qdCount = 1, anCount = 1 }
        , question = [q]
        , answer = [an]
      }

responseAAAA :: Word16 -> Question -> IPv6 -> DNSFormat
responseAAAA ident q ip =
  let hd = header defaultResponse
      dom = qname q
      an = ResourceRecord dom AAAA IN 300 16 (RD_AAAA ip)
  in  defaultResponse {
          header = hd { identifier=ident, qdCount = 1, anCount = 1 }
        , question = [q]
        , answer = [an]
      }
