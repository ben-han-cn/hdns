import qualified Network.ZDNS as DNS
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString
import Network.BSD
import Data.Word
import System.Environment
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import qualified Data.ByteString.Lazy as BLL
import Data.ByteString.Lazy hiding (putStrLn, filter, length)

main :: IO ()
main = withSocketsDo $ do
    [ip , n, t] <- getArgs
    case (DNS.mkDomain n, read t) of
        (Just domain, qtype) -> doQuery ip domain qtype
        (Nothing, _) -> putStrLn $ "name isn't valid: " ++ n


doQuery :: String -> DNS.Domain -> DNS.RRType -> IO ()
doQuery ip n t = 
    DNS.withResolver ip (\resolver -> do
        response <- DNS.doQuery resolver n t
        case response of
            (Right msg) -> print  msg
            (Left error) -> print $ "error:\n" ++ error)
