-- | Data types for DNS Query and Response.
--   For more information, see <http://www.ietf.org/rfc/rfc1035>.

module Network.HDNS.Types (
  -- * Domain
    Domain
  , parentDomain
  , (+++)
  , rootDomain
  , popLabel

  , TYPE (..) 
  , Klass (..)
  , fromWord16, toWord16

  , DNSError (..)
  , DNSFormat (..)
  , DNSHeader (..)

  , DNSFlags (..)
  , QorR (..)
  , OPCODE (..)
  , RCODE (..)
  , ResourceRecord (..)
  , Question (..)
  , RDATA (..)
  , responseA, responseAAAA
  , fromString, toString
  ) where

import Network.HDNS.Internal
import Network.HDNS.Util
