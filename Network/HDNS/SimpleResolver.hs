module Network.HDNS.SimpleResolver (
    SimpleResolver(..)
 ,  makeResolver
 ,  withResolver
 ,  doQuery 
 ,  doQuery'
)where

import Network.HDNS.Internal 
import Network.HDNS.Util (fromString, intToWord)
import Network.HDNS.Decode (decode)
import Network.HDNS.Encode (composeQuery)
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS 
import qualified Data.ByteString.Lazy as BL 
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString 
import Network.BSD
import Ben.Util.Number as Num
import System.IO.Unsafe (unsafePerformIO)
import Data.Functor ((<$>))
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)

data SimpleResolver = SR Socket

makeResolver :: IP -> IO SimpleResolver
makeResolver ip =  do
    addr <- ipToSockAddr ip
    sock <- socket AF_INET Datagram defaultProtocol
    connect sock addr
    return $ SR sock

withResolver :: IP -> (SimpleResolver -> IO a) -> IO a
withResolver ip fun = runResourceT $ do
    (resolverID, resolver) <- allocate (makeResolver ip) releaseResolver
    result <- lift $ fun resolver
    release resolverID
    return result

releaseResolver :: SimpleResolver -> IO ()
releaseResolver (SR sock) = sClose sock
    
type IP = String 
ipToSockAddr :: IP -> IO SockAddr
ipToSockAddr ip = do
    proto <- getProtocolNumber "udp"
    let hints = defaultHints {
            addrFlags = [AI_ADDRCONFIG, AI_NUMERICHOST, AI_PASSIVE]
          , addrSocketType = Datagram
          , addrProtocol = proto
          }   
    a:_ <- getAddrInfo (Just hints) (Just ip) (Just "domain")
    return $ addrAddress a

doQuery :: SimpleResolver -> Domain -> TYPE -> IO (Either String DNSFormat)
doQuery res@(SR sock) n t = do
    doQuery' res n t
    (bs, _) <- recvFrom sock 512
    return $ (decode . BL.fromChunks) [bs]

doQuery' :: SimpleResolver -> Domain -> TYPE -> IO ()
doQuery' (SR sock) n t = do
    sendAll sock $ generateQueryMsg n t

qid :: IO Word16
qid = intToWord <$> Num.rand 65535

generateQueryMsg :: Domain -> TYPE -> B.ByteString
generateQueryMsg n t = 
    BS.concat . BL.toChunks $ composeQuery (unsafePerformIO qid) 
                                              [Question n t IN]
