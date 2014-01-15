-- | Data types for DNS Query and Response.
--   For more information, see <http://www.ietf.org/rfc/rfc1035>.

module Network.HDNS.Types (
  -- * Domain
    Domain
  -- * TYPE
  , TYPE (..) 
  , Klass (..)
  , fromWord16, toWord16, fromString
  -- * DNS Error
  , DNSError (..)
  -- * DNS Format
  , DNSFormat (..)
  -- * DNS Header
  , DNSHeader (..)
  -- * DNS Flags
  , DNSFlags (..)
  -- * DNS Body
  , QorR (..)
  , OPCODE (..)
  , RCODE (..)
  , ResourceRecord (..)
  , Question (..)
  , RDATA (..)
  , responseA, responseAAAA
  ) where

import Network.HDNS.Internal
import Network.HDNS.Util
