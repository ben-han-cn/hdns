{-# LANGUAGE RecordWildCards #-}
module Network.ZDNS.SimpleResolver (
    SimpleResolver(..)
 ,  makeResolver
 ,  withResolver
 ,  doQuery 
 ,  doQuery'
)where

import Network.ZDNS.Types.Message
import Network.ZDNS.Types.EDNS
import Network.ZDNS.Types.RRset
import Network.ZDNS.Types.Name
import Network.ZDNS.Util
import Network.ZDNS.MessageRender
import Data.Word
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS 
import qualified Data.ByteString.Lazy as BL 
import qualified Blaze.ByteString.Builder as BB
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString 
import Ben.Util.Number as Num
import System.IO.Unsafe (unsafePerformIO)
import Data.Functor ((<$>))
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)

type IP = String 
data SimpleResolver = SR {
    srSocket :: Socket
   ,srServerIP :: IP
   ,srPort :: Int
}

makeResolver :: IP -> IO SimpleResolver
makeResolver ip =  do
    sock <- createSocket ip Datagram
    return $ SR sock ip 53

createSocket :: IP -> SocketType -> IO Socket
createSocket ip stype = do
    addr <- ipToSockAddr ip
    sock <- socket AF_INET stype defaultProtocol
    connect sock addr
    return sock

fallbackTCP :: SimpleResolver -> IO SimpleResolver
fallbackTCP sr = do
    let serverIP = srServerIP sr
    sock <- createSocket serverIP Stream
    return $ SR sock serverIP 53

withResolver :: IP -> (SimpleResolver -> IO a) -> IO a
withResolver ip fun = runResourceT $ do
    (resolverID, resolver) <- allocate (makeResolver ip) releaseResolver
    result <- lift $ fun resolver
    release resolverID
    return result

releaseResolver :: SimpleResolver -> IO ()
releaseResolver sr = sClose $ srSocket sr
    
ipToSockAddr :: IP -> IO SockAddr
ipToSockAddr ip = do
    let hints = defaultHints {
            addrFlags = [AI_ADDRCONFIG, AI_NUMERICHOST, AI_NUMERICSERV]
          }   
    a:_ <- getAddrInfo (Just hints) (Just ip) (Just "53")
    return $ addrAddress a

doQuery :: SimpleResolver -> Domain -> RRType -> IO (Either String Message)
doQuery res n t = do
    doQuery' res n t
    (bs, _) <- recvFrom (srSocket res) 512
    case parse  readMessage $ BL.fromChunks [bs] of
        Left e -> return $ Left e
        Right (m, _) -> if getTC . flags . header $ m 
                        then queryUsingTCP res n t
                        else return $ Right m

queryUsingTCP :: SimpleResolver -> Domain -> RRType -> IO (Either String Message)
queryUsingTCP res n t = do
    newRes <- fallbackTCP res
    let message = generateQueryMsg n t
    sendAll (srSocket newRes) (BB.toByteString . BB.fromWrite . BB.writeWord16be. fromIntegral . B.length $ message)
    sendAll (srSocket newRes) message
    (len, _) <- recvFrom (srSocket newRes) 2
    (bs,_) <- recvFrom (srSocket newRes) (fromIntegral . Num.ntohs . B.unpack $ len)
    releaseResolver newRes
    case parse  readMessage $ BL.fromChunks [bs] of
        Left e -> return $ Left e
        Right (m, _) -> return $ Right m

doQuery' :: SimpleResolver -> Domain -> RRType -> IO ()
doQuery' sr n t = do
    sendAll (srSocket sr) $ generateQueryMsg n t

qid :: IO Word16
qid = intToWord <$> Num.rand 65535

generateQueryMsg :: Domain -> RRType -> B.ByteString
generateQueryMsg n t = 
    BS.concat . BL.toChunks $ composeQuery (unsafePerformIO qid) 
                                           (Question n t IN)

composeQuery :: Word16 -> Question -> BL.ByteString
composeQuery qid' q = 
    rend $ writeMessage message
    where 
        defaultFlag = setRD True 0
        defaultHeader = Header qid' defaultFlag 1 0 0 1
        message = Message defaultHeader q V.empty V.empty (V.fromList [createOPTRRset 4096 True])
