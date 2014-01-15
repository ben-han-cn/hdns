import qualified Network.HDNS as DNS
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString
import Network.BSD
import Data.Word
import System.Environment
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import qualified Data.ByteString.Lazy as BLL
import Data.ByteString.Lazy hiding (putStrLn, filter, length)

qid :: Word16
qid = 10

stringToDomain :: String -> DNS.Domain
stringToDomain str = B.pack str

composeQuestion :: String -> DNS.TYPE -> B.ByteString
composeQuestion name t = 
    let question = DNS.Question (stringToDomain name) t DNS.IN 
        in B.concat . BLL.toChunks $ DNS.composeQuery qid [question]


makeAddrInfo :: String -> IO AddrInfo
makeAddrInfo addr = do
    proto <- getProtocolNumber "udp"
    let hints = defaultHints {
            addrFlags = [AI_ADDRCONFIG, AI_NUMERICHOST, AI_PASSIVE]
          , addrSocketType = Datagram
          , addrProtocol = proto
          }   
    a:_ <- getAddrInfo (Just hints) (Just addr) (Just "domain")
    return a

main :: IO ()
main = withSocketsDo $ do
    [server_ip, qname, qtype] <- getArgs
    s <- socket AF_INET Datagram defaultProtocol
    addr <- makeAddrInfo server_ip
    sendAllTo s (composeQuestion qname (DNS.fromString qtype)) (addrAddress addr)
    (bs, _) <- recvFrom s 512
    case DNS.decode (fromChunks [bs]) of
        (Right msg) -> print  msg
        (Left error) -> print $ "error:\n" ++ error
