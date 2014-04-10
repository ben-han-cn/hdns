-- | Data types for DNS Query and Response.
--   For more information, see <http://www.ietf.org/rfc/rfc1035>.

module Network.ZDNS.Types (
    Domain
  , mkDomain
  , domainLength
  , labelCount
  , rootDomain
  , splitDomain
  , DomainRelation(..)
  , DomainCompResult(..)
  , compareDomain
  , concatDomain
  , superDomain

-- type
  , RRType(..)
  , RCode(..)
  , OpCode(..)
  , RRClass(..)
  , Question(..)
  , Header(..)
  , Message(..)
  , getOpCode
  , getRCode
  , getQR
  , getAA
  , getCD
  , getAD
  , getRA
  , getRD
  , getTC

  , RRset(..)
  , RdataField(..)
  , RdataFieldType(..)
  ) where

import Network.ZDNS.Types.Name
import Network.ZDNS.Types.RRset
import Network.ZDNS.Types.Message
