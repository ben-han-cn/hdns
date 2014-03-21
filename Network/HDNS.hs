-- | A thread-safe DNS library for both clients and servers written
--   in pure Haskell.
--   The Network.DNS module re-exports all other exposed modules for
--   convenience.
--   Applications will most likely use the high-level interface, while
--   library/daemon authors may need to use the lower-level one.
--
module Network.HDNS (
    module Network.HDNS.Types
  , module Network.HDNS.Decode
  , module Network.HDNS.Encode
  , module Network.HDNS.SimpleResolver
  ) where

import Network.HDNS.Types
import Network.HDNS.Decode
import Network.HDNS.Encode
import Network.HDNS.SimpleResolver
