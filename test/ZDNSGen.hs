module ZDNSGen(
    labelGen
 ,  nameGen
 ,  relativeNameGen 
 ,  absoluteNameGen
 ,  domainGen
 ,  popularDomainGen
 ,  domainListGen
 ) where
 
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
 
import qualified Network.ZDNS as DNS
import qualified Data.List as L
import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)

labelGen :: Gen String
labelGen = do
    label <- listOf1 $ elements ['a'..'z']
    return $ take 62 label

absoluteNameGen :: Gen String
absoluteNameGen = do 
    name <- relativeNameGen
    return $ name ++ "."

relativeNameGen :: Gen String
relativeNameGen = do 
    label_count <- choose (1, 126)
    label_strs <- replicateM label_count labelGen
    return $ toRelative $ take 253 $ DNS.join "." label_strs
    where toRelative s = if last s == '.' then init s else s

nameGen :: Gen String
nameGen = oneof [absoluteNameGen, relativeNameGen]

domainGen :: Gen DNS.Domain
domainGen = fromJust . DNS.mkDomain <$> nameGen

domainListGen :: Int -> Int -> Gen DNS.Domain -> Gen [DNS.Domain]
domainListGen minl maxl gen = do
    domain_count <- choose (minl, maxl)
    replicateM domain_count gen


topDomainNames :: [String]
topDomainNames = [
    "google.com",
    "facebook.com",
    "youtube.com",
    "yahoo.com",
    "baidu.com",
    "wikipedia.org",
    "qq.com",
    "live.com",
    "taobao.com",
    "linkedin.com",
    "sina.com.cn",
    "twitter.com",
    "amazon.com",
    "google.co.in",
    "blogspot.com",
    "weibo.com",
    "wordpress.com",
    "yahoo.co.jp",
    "bing.com",
    "tmall.com",
    "yandex.ru",
    "vk.com",
    "ebay.com",
    "google.de",
    "babylon.com",
    "msn.com",
    "google.co.uk",
    "soso.com",
    "google.fr",
    "tumblr.com",
    "googleusercontent.com",
    "mail.ru",
    "pinterest.com",
    "google.co.jp",
    "apple.com",
    "google.com.br",
    "microsoft.com",
    "google.ru",
    "google.es",
    "google.it",
    "xhamster.com",
    "blogger.com",
    "imdb.com",
    "craigslist.org",
    "xvideos.com",
    "ask.com",
    "sohu.com",
    "conduit.com",
    "bbc.co.uk",
    "go.com",
    "amazon.co.jp",
    "google.com.mx",
    "odnoklassniki.ru",
    "google.ca",
    "youku.com",
    "livejasmin.com",
    "amazon.de",
    "adobe.com",
    "flickr.com",
    "avg.com",
    "ifeng.com",
    "t.co",
    "pornhub.com",
    "aol.com",
    "rakuten.co.jp",
    "cnn.com",
    "thepiratebay.sx",
    "mywebsearch.com",
    "ebay.de",
    "amazon.co.uk",
    "adf.ly",
    "alibaba.com",
    "espn.go.com",
    "blogspot.in",
    "google.com.tr",
    "redtube.com",
    "google.co.id",
    "instagram.com",
    "huffingtonpost.com",
    "alipay.com",
    "about.com",
    "stackoverflow.com",
    "sogou.com",
    "google.com.au",
    "livedoor.com",
    "ebay.co.uk",
    "netflix.com",
    "dailymotion.com",
    "imgur.com",
    "zedo.com",
    "google.pl"
    ]

popularDomainGen :: Gen DNS.Domain
popularDomainGen = fromJust . DNS.mkDomain <$> elements topDomainNames


