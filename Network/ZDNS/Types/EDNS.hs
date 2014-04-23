module Network.ZDNS.Types.EDNS where

import Network.ZDNS.Types.Name
import Network.ZDNS.Types.RRset
import Network.ZDNS.Util
import Data.Word 
import qualified Data.ByteString as BS 
import Data.Bits (shiftL)
import qualified Data.Vector as V

createOPTRRset :: Word16 -> Bool -> RRset
createOPTRRset ps dd = RRset rootDomain OPT (UNKNOWNKLASS ps) ttl (V.fromList [V.fromList [RDFBinary BS.empty]])
                        where ttl = if dd 
                                    then (1 `shiftL` 15) 
                                    else 0

