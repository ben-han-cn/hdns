{-# LANGUAGE CPP #-}
-- | A thread-safe DNS library for both clients and servers written
--   in pure Haskell.
--   The Network.DNS module re-exports all other exposed modules for
--   convenience.
--   Applications will most likely use the high-level interface, while
--   library/daemon authors may need to use the lower-level one.
--
module Network.ZDNS(
    module Network.ZDNS.Types
 ,  module Network.ZDNS.SimpleResolver
-- #ifdef TEST
 ,  R.Parser
 ,  R.parse
 ,  R.readDomain
 ,  R.readQuestion
 ,  R.readHeader
 ,  R.readMessage
-- #endif
 ,  R.rend
 ,  R.writeDomain
 ,  R.writeQuestion
 ,  R.writeHeader
 ,  R.writeMessage

  ) where

import Network.ZDNS.Types
import Network.ZDNS.SimpleResolver
import qualified Network.ZDNS.MessageRender as R
