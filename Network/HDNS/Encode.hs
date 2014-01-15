{-# LANGUAGE RecordWildCards #-}

module Network.HDNS.Encode (
    encode
  , composeQuery
  ) where

import qualified Blaze.ByteString.Builder as BB (toByteString, fromWrite, writeInt16be)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import qualified Data.ByteString.Char8 as BS (length, null, break, drop)
import Network.HDNS.Util (toWord16, intToWord)
import Network.HDNS.MessageRender
import Network.HDNS.Internal
import Data.Monoid
import Control.Monad.State
import Data.Bits
import Data.Word
import Data.IP

----------------------------------------------------------------

-- | Composing query. First argument is a number to identify response.

composeQuery :: Word16 -> [Question] -> BL.ByteString
composeQuery idt qs = encode qry
  where
    hdr = header defaultQuery
    qry = defaultQuery {
        header = hdr {
           identifier = idt
         , qdCount = intToWord . length $ qs
         }
      , question = qs
      }

----------------------------------------------------------------

-- | Composing DNS data.

encode :: DNSFormat -> BL.ByteString
encode fmt = rend (rendDNSFormat fmt)

----------------------------------------------------------------

rendDNSFormat :: DNSFormat -> Render
rendDNSFormat fmt = rendHeader hdr
                  <> mconcat (map rendQuestion qs)
                  <> mconcat (map rendRR an)
                  <> mconcat (map rendRR au)
                  <> mconcat (map rendRR ad)
  where
    hdr = header fmt
    qs = question fmt
    an = answer fmt
    au = authority fmt
    ad = additional fmt

rendHeader :: DNSHeader -> Render
rendHeader hdr = writeUint16 (identifier hdr)
               <> rendFlags (flags hdr)
               <> writeUint16 (qdCount hdr)
               <> writeUint16 (anCount hdr)
               <> writeUint16 (nsCount hdr)
               <> writeUint16 (arCount hdr)

rendFlags :: DNSFlags -> Render
rendFlags DNSFlags{..} = writeUint16 word
  where
    word16 :: Enum a => a -> Word16
    word16 = toEnum . fromEnum

    set :: Word16 -> State Word16 ()
    set byte = modify (.|. byte)

    st :: State Word16 ()
    st = sequence_
              [ set (word16 rcode)
              , when recAvailable        $ set (bit 7)
              , when recDesired          $ set (bit 8)
              , when trunCation          $ set (bit 9)
              , when authAnswer          $ set (bit 10)
              , set (word16 opcode `shiftL` 11)
              , when (qOrR==QR_Response) $ set (bit 15)
              ]

    word = execState st 0

rendQuestion :: Question -> Render
rendQuestion Question{..} =
        writeDomain qname
    <> writeUint16 (toWord16 qtype)
    <> writeUint16 (toWord16 qklass)

rendRR :: ResourceRecord -> Render
rendRR ResourceRecord{..} =
    mconcat
      [ writeDomain rrname
      , writeUint16 (toWord16 rrtype)
      , writeUint16 (toWord16 rrklass)
      , writeUint32   rrttl
      , rlenRDATA
      ]
  where
    rlenRDATA = do
        moveWritePos 2
        rDataWrite <- rendRDATA rdata
        let rdataLength = fromIntegral . BS.length . BB.toByteString . BB.fromWrite $ rDataWrite
        moveWritePos (-2)
        (writeUint16 rdataLength) <> (return rDataWrite)

rendRDATA :: RDATA -> Render
rendRDATA rd = case rd of
    (RD_A ip)          -> mconcat $ map (writeUint8 . intToWord)  (fromIPv4 ip)
    (RD_AAAA ip)       -> mconcat $ map (writeUint16 . intToWord) ( fromIPv6 ip)
    (RD_NS dom)        -> writeDomain dom
    (RD_CNAME dom)     -> writeDomain dom
    (RD_PTR dom)       -> writeDomain dom
    (RD_MX prf dom)    -> mconcat [writeUint16 prf, writeDomain dom]
    (RD_TXT txt)       -> writeData txt
    (RD_OTH bytes)     -> mconcat $ map writeUint8 bytes
    (RD_SOA d1 d2 serial refresh retry expire min') -> mconcat
        [ writeDomain d1
        , writeDomain d2
        , writeUint32 serial
        , writeUint32 refresh
        , writeUint32 retry
        , writeUint32 expire
        , writeUint32 min'
        ]
    (RD_SRV prio weight port dom) -> mconcat
        [ writeUint16 prio
        , writeUint16 weight
        , writeUint16 port
        , writeDomain dom
        ]

----------------------------------------------------------------
