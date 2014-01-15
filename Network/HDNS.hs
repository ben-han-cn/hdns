-- | A thread-safe DNS library for both clients and servers written
--   in pure Haskell.
--   The Network.DNS module re-exports all other exposed modules for
--   convenience.
--   Applications will most likely use the high-level interface, while
--   library/daemon authors may need to use the lower-level one.
--
module Network.HDNS (
  -- * High level
  module Network.HDNS.Types
  -- | All of the types that the other modules use.

  -- * Low level
  , module Network.HDNS.Decode
  -- | Decoding a response.

  , module Network.HDNS.Encode
  -- | Encoding a query.

  ) where

import Network.HDNS.Types
import Network.HDNS.MessageRender
import Network.HDNS.Decode
import Network.HDNS.Encode
