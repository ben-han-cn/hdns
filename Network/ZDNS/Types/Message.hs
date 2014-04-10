{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module Network.ZDNS.Types.Message where

import Data.Word
import Data.List (intercalate)
import qualified Data.Vector as V
import Network.ZDNS.Types.RRset
import Network.ZDNS.Types.Name
import Network.ZDNS.Util

----------------------------------------------------------------
data HeaderFlag = QR | AA | TC | RD | RA | AD | CD

data RCode =  NOERROR 
            | FORMERR 
            | SERVFAIL 
            | NXDOMAIN
            | NOTIMP 
            | REFUSED
            | YXDOMAIN
            | YXRRSET 
            | NXRRSET
            | NOTAUTH
            | NOTZONE deriving (Eq, Enum, Show)

data OpCode = QUERY
            | IQUERY
            | STATUS
            | RESERVED3
            | NOTIFY
            | UPDATE deriving (Eq, Enum, Show)

data Header = Header {
    hid        :: Word16
 ,  flags      :: Word16
 ,  qcount     :: Word16
 ,  anscount   :: Word16
 ,  authcount  :: Word16
 ,  addicount  :: Word16
} 

instance Show Header where
    show Header{..} =
            ";; ->>HEADER<<- " ++
            "opcode: " ++ (show $ getOpCode flags) ++ "," ++
            "status: " ++ (show $ getRCode flags) ++ "," ++
            "id: " ++ (show hid) ++ "\n" ++
            ";; flags: " ++ showFlags ++ "; " ++
            "QUERY: " ++ (show qcount) ++ ", " ++
            "ANSWER: " ++ (show anscount) ++ ", " ++
            "AUTHORITY: " ++ (show authcount) ++ ", " ++
            "ADDITIONAL: " ++ (show addicount) ++ "\n"
        where 
            showBoolFlag a s = if a then s else ""
            showFlags = intercalate " " $ filter (\t -> t /= "") 
                                            [showBoolFlag (getQR flags) "qr" 
                                            ,showBoolFlag (getAA flags) "aa"
                                            ,showBoolFlag (getTC flags) "tc"
                                            ,showBoolFlag (getRD flags) "rd"
                                            ,showBoolFlag (getRA flags) "ra"]
-- +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
-- |QR|   Opcode  |AA|TC|RD|RA|  |AD|CD|   RCODE   |
-- +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
-- define flag descriptor :: FlagDescriptor (width, pos)
qrField :: FlagDescriptor
qrField = FlagDescriptor 1 15
opcodeField :: FlagDescriptor
opcodeField = FlagDescriptor 4 11
aaField :: FlagDescriptor
aaField = FlagDescriptor 1 10 
tcField :: FlagDescriptor
tcField = FlagDescriptor 1 9
rdField :: FlagDescriptor
rdField = FlagDescriptor 1 8
raField :: FlagDescriptor
raField = FlagDescriptor 1 7
adField :: FlagDescriptor
adField = FlagDescriptor 1 5
cdField :: FlagDescriptor
cdField = FlagDescriptor 1 4
rcodeField :: FlagDescriptor
rcodeField = FlagDescriptor 4 0

getQR :: Word16 -> Bool
getQR flags = getFlag flags qrField == 1
setQR :: Bool -> Word16 -> Word16
setQR isquery flags = setFlag flags qrField 
                        (if isquery then 0 else 1)

getOpCode :: Word16 -> OpCode
getOpCode flags = toEnum $ getFlag flags opcodeField
setOpCode :: OpCode -> Word16 -> Word16
setOpCode c flags = setFlag flags opcodeField (fromEnum c)

getAA :: Word16 -> Bool
getAA flags = getFlag flags aaField == 1
setAA :: Bool -> Word16 -> Word16
setAA isaa flags = setFlag flags aaField
                        (if isaa then 0 else 1)

getTC :: Word16 -> Bool
getTC flags = getFlag flags tcField == 1
setTC :: Bool -> Word16 -> Word16
setTC istc flags = setFlag flags tcField
                        (if istc then 0 else 1)

getRD :: Word16 -> Bool
getRD flags = getFlag flags rdField == 1
setRD :: Bool -> Word16 -> Word16
setRD isrd flags = setFlag flags rdField
                        (if isrd then 0 else 1)

getRA :: Word16 -> Bool
getRA flags = getFlag flags raField == 1
setRA :: Bool -> Word16 -> Word16
setRA isra flags = setFlag flags raField
                        (if isra then 0 else 1)

getAD :: Word16 -> Bool
getAD flags = getFlag flags adField == 1
setAD :: Bool -> Word16 -> Word16
setAD isad flags = setFlag flags adField
                        (if isad then 0 else 1)

getCD :: Word16 -> Bool
getCD flags = getFlag flags cdField == 1
setCD :: Bool -> Word16 -> Word16
setCD iscd flags = setFlag flags cdField
                        (if iscd then 0 else 1)

getRCode :: Word16 -> RCode
getRCode flags = toEnum $ getFlag flags rcodeField
setRCode :: OpCode -> Word16 -> Word16
setRCode c flags = setFlag flags rcodeField (fromEnum c)

type Section = V.Vector RRset
data Question = Question {
    qname  :: Domain
 ,  qtype  :: RRType
 ,  qclass :: RRClass
}

addRRToSection :: RRset -> Section -> Section
addRRToSection rrset sec = 
    case V.findIndex (\rs -> and [(rrsetName rrset == rrsetName rs)
                                  ,(rrsetType rrset == rrsetType rs)]) sec of
        Just i -> sec V.// [(i, mergedRRset i)]
        Nothing -> V.snoc sec rrset
    where mergedRRset index = 
            let (RRset n t c ttl rdatas) = sec V.! index
                in RRset n t c (min ttl $ rrsetTTL rrset) (rdatas V.++ (rrsetRdatas rrset))
                    

instance Show Question where
    show Question{..} = 
        (intercalate "\t" [show qname, show qclass, show qtype]) ++ "\n"

data Message = Message {
    header :: Header
 ,  question :: Question
 ,  answer:: Section
 ,  authority:: Section
 ,  additional:: Section
} 

instance Show Message where
    show Message{..} = 
        intercalate "\n" [(show header)
        ,";; QUESTION SECTION:"
        ,(show question)
        ,";; ANSWER SECTION:"
        ,(showSection answer)
        ,";; AUTHORITY SECTION:"
        ,(showSection authority)
        ,";; ADDITIONAL SECTION:"
        ,(showSection additional)]
        where showSection s = intercalate "\n" $ V.toList $ V.map show s

