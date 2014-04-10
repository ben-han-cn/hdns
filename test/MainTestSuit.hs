module Main (
    main
 ) where
 
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Data.List
import Data.Maybe
import Data.Word
 
import Network.ZDNS
import Ben.Util.String (join)
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import DomainTreeProp
import ZDNSGen


labelCount' :: String -> Int
labelCount' s = length $ elemIndices '.' s

propCreateDomain = forAll relativeNameGen $ \name ->
                    case mkDomain name of
                        Nothing -> False
                        Just domain -> (labelCount domain) == labelCount' name + 2

propShowDomain = forAll relativeNameGen $ \name -> 
                         case mkDomain name of
                            Nothing -> False
                            Just domain -> (show domain) == (name ++ ".")

domainCompareData :: [(String, String, DomainCompResult)]
domainCompareData = [("www.baidu.com", "www.baidu.Com.", DCR EQ 4 EQUALDOMAIN)
                    ,("WWW.baidu.com", "www.baidu", DCR GT 1 NORELATION)
                    ,("WWW.baidu.com.", "baidu.com", DCR GT 3 SUBDOMAIN)
                    ,("BAIDU.COM.", "aaa.baidu.com", DCR LT 3 SUPERDOMAIN)
                    ,("BAIDU.CN.", "aaa.baidu.com", DCR LT 1 NORELATION)
                    ,("A.COM", "B.COM.", DCR LT 2 COMMONANCESTOR)]

propCompareDomain = and $ map checkCompareResult domainCompareData 
                    where checkCompareResult (s1, s2, result) = 
                            let Just n1 = mkDomain s1
                                Just n2 = mkDomain s2
                                in compareDomain n1 n2 == result


domainSplitData :: [(String, Int, Int, Maybe String)]
domainSplitData =  [("www.baidu.com", 0, 1, Just "www")
                   ,("www.baidu.com", 1, 2, Just "baidu.com")
                   ,("www.baidu.com", 1, 3, Just "baidu.com.")
                   ,("www.baidu.com", 1, 4, Nothing)
                   ,("www.baidu.com", 2, 1, Just "com.")
                   ,("www.baidu.com", 2, 2, Just "com.")
                   ,("www.baidu.com", 2, 3, Nothing)]
propSplitDomain = and $ map checkSplitResult domainSplitData
                  where checkSplitResult (s1, start, len, result) = 
                            case result of
                                Just s2 -> ((splitDomain start len). fromJust . mkDomain $ s1) == mkDomain s2
                                Nothing -> ((splitDomain start len). fromJust . mkDomain $ s1) == Nothing


domainConcatData :: [(String, String, String)]
domainConcatData =  [("www.baidu.com", ".", "www.baidu.com")
                    ,("www.baidu.", "com", "www.baidu.com.")
                    ,("www.baidu.com", "com", "www.baidu.com.com")
                    ,("www.baidu.com", "Com.", "www.baidu.com.com.")
                    ,(".", "Com.", "com")
                    ,(".", ".", ".")]

propConcatDomain = and $ map checkConcatResult domainConcatData 
                  where checkConcatResult (s1, s2, s3) =
                            concatDomain (unsafeMkDomain s1) (unsafeMkDomain s2) == mkDomain s3
                        unsafeMkDomain str = fromJust $ mkDomain str
 
 
domainSuperData :: [(String, Int, Maybe String)]
domainSuperData =  [("www.baidu.com", 0, Just "www.baidu.com")
                    ,("www.baidu.", 1, Just "baidu")
                    ,("com", 1, Just ".")
                    ,("com", 2, Nothing)
                    ,(".", 1, Nothing)]
propSuperDomain = and $ map checkSuperResult domainSuperData 
                  where checkSuperResult (s1, level, s3) =
                            superDomain level (unsafeMkDomain s1) == destDomain s3
                        unsafeMkDomain str = fromJust $ mkDomain str
                        destDomain Nothing = Nothing
                        destDomain (Just str) = mkDomain str

domainRawData :: [([Word8], String, Bool)]
domainRawData = [([3, 119, 119, 119, 5, 98, 97, 105, 100, 117, 3, 99, 111, 109, 0], "www.baidu.com", True)
                ,([3, 119, 119, 119, 5, 98, 97, 105, 100, 117, 3, 99, 111, 109, 0, 2], "www.baidu.com.", True)
                ,([3, 119, 119, 119, 0], "www.", True)
                ,([3, 119, 119, 119, 1, 0], "", False)
                ,([100, 119, 119, 119, 0], ".", False)
                ,([4, 119, 119, 119, 0], "www.", False)
                ,([3, 119, 119, 119, 5, 98, 97, 105, 100, 117, 3, 99, 111, 109], "", False)]
 
propParseDomain = and $ map checkParseResult domainRawData
                  where checkParseResult (bs, expect, isOk) = 
                            case parse readDomain (BL.fromStrict . B.pack $ bs) of
                                Right (d, _) -> and [d == (fromJust $ mkDomain expect), isOk]
                                Left _ -> isOk == False

                
questionRawData :: [Word8]
questionRawData = [3, 119, 119, 119, 5, 98, 97, 105, 100, 117, 3, 99, 111, 109, 0, 0, 1, 0, 1]
propParseQuestion = case parse readQuestion (BL.fromStrict . B.pack $ questionRawData) of
                        Right(q, _) -> and [qtype q == A 
                                           ,qname q == (fromJust $ mkDomain "www.baidu.com") 
                                           ,qclass q == IN]


messageHeaderRawData :: [Word8]
messageHeaderRawData = [251, 27, 128, 144, 0, 1, 0, 0, 0, 0, 0, 0]
propParseHeader = case parse readHeader (BL.fromStrict . B.pack $ messageHeaderRawData) of
                        Right(h, _) -> and [hid h == 64283
                                           ,getRCode (flags h) == NOERROR
                                           ,getOpCode (flags h) == QUERY
                                           ,getRD (flags h) == False
                                           ,getAD (flags h) == False
                                           ,getAA (flags h) == False
                                           ,getTC (flags h) == False
                                           ,getRA (flags h)
                                           ,getQR (flags h)
                                           ,getCD (flags h)
                                           ,qcount h == 1]


--;; id = 24545
--;; qr = true    opcode = Query    aa = false    tc = false    rd = false
--;; ra = true    ad = false    cd = false    rcode  = NOERROR
--;; qdcount = 1  ancount = 1  nscount = 0  arcount = 0
--
--;; QUESTION SECTION (1  record)
--;; 66e.cc.  IN  A
--
--;; ANSWER SECTION (1  record)
--66e.cc. 481 IN  A   183.86.193.81
messageRawData :: [Word8]
messageRawData = [95, 225, 128, 128, 0, 1, 0, 1, 0, 0, 0, 0, 3, 54, 54, 101, 2, 99, 99, 0, 0, 1, 0, 1, 192, 12, 0, 1, 0, 1, 0, 0, 1, 225, 0, 4, 183, 86, 193, 81]
propParseMessage = case parse readMessage (BL.fromStrict . B.pack $ messageRawData) of
                        Right(m, _) -> and [(hid . header $ m) == 24545
                                           ,(anscount . header $ m) == 1]



messageRawData2 :: [Word8]
messageRawData2 = [248, 0, 129, 128, 0, 1, 0, 18, 0, 0, 0, 0, 4, 110, 101, 119, 115, 4, 115, 105, 110, 97, 3, 99, 111, 109, 2, 99, 110, 0, 0, 1, 0, 1, 192, 12, 0, 5, 0, 1, 0, 0, 0, 14, 0, 10, 7, 106, 117, 112, 105, 116, 101, 114, 192, 17, 192, 46, 0, 5, 0, 1, 0, 0, 13, 226, 0, 6, 3, 97, 114, 97, 192, 17, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 183, 60, 187, 38, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 183, 60, 187, 39, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 183, 60, 187, 40, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 183, 60, 187, 41, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 183, 60, 187, 42, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 183, 60, 187, 43, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 183, 60, 187, 44, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 183, 60, 187, 45, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 183, 60, 187, 46, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 58, 63, 236, 31, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 58, 63, 236, 32, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 58, 63, 236, 33, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 58, 63, 236, 34, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 58, 63, 236, 35, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 58, 63, 236, 37, 192, 68, 0, 1, 0, 1, 0, 0, 0, 14, 0, 4, 58, 63, 236, 38]
propRenderMessage = case parse readMessage (BL.fromStrict . B.pack $ messageRawData2) of
                        Right(m, _) -> (B.unpack . BL.toStrict $ rend (writeMessage m)) == messageRawData2

                                           
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
  [
    testGroup "Test domain"
    [
        testProperty "create domain with specified label count" propCreateDomain
     ,  testProperty "show domain always in absolute format" propShowDomain
     ,  testProperty "domain compare" propCompareDomain
     ,  testProperty "split domain" propSplitDomain
     ,  testProperty "concat domain" propConcatDomain
     ,  testProperty "super domain" propSuperDomain
    ]
   
    ,testGroup "parse message"
    [
        testProperty "parseDomain" propParseDomain
     ,  testProperty "parseQuestion" propParseQuestion
     ,  testProperty "parseHeader" propParseHeader
     ,  testProperty "parse whole message" propParseMessage
     ,  testProperty "render whole message" propRenderMessage
    ]

    ,testGroup "domain tree"
    [
        testProperty "domain tree to list" propTreeToList
     ,  testProperty "search less equal " propSearchLessEqual
    ]
 ]
