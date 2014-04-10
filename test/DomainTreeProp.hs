module DomainTreeProp(
    propTreeToList
 ,  propSearchLessEqual
 ) where
 
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
 
import qualified Network.ZDNS as DNS
import qualified Data.Tree.RBTree as RBT
import qualified Data.List as L
import Data.Maybe (fromJust)
import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import ZDNSGen

mkDomainTree :: [DNS.Domain] -> RBT.RBTree DNS.Domain
mkDomainTree domains = RBT.insertOrdList RBT.emptyRB $ L.sort domains

propTreeToList = forAll (domainListGen 0 100 domainGen) $ \names ->
                    (RBT.toList $ mkDomainTree names) == (L.sort names)


domainListAndOneNumberGen minLen maxLen 
    | minLen < 2 = domainListAndOneNumberGen 2 maxLen
    | otherwise =  do
                        domains <- L.nub <$> domainListGen minLen maxLen popularDomainGen
                        if (length domains) < minLen    
                        then domainListAndOneNumberGen minLen maxLen
                        else
                            do 
                                index <- choose (1, length domains - 1)
                                return (domains, index)

domainTestData = ["a",  "b", "c", "z.d.e.f", "g.h", "o.w.y.d.e.f", "q.w.y.d.e.f"]
propSearchLessEqual = and [(searchLessEqual "b") == (Nothing, Just $ strToDomain "b")
                          ,(searchLessEqual "d.e.f") == (Just $ strToDomain "c", Nothing)
                          ,(searchLessEqual "p.w.y.d.e.f") == (Just $ strToDomain "o.w.y.d.e.f.", Nothing)
                          ]
                      where 
                            strToDomain = fromJust . DNS.mkDomain 
                            domainTree = mkDomainTree $ map strToDomain domainTestData
                            searchLessEqual n = RBT.searchLessEqual compare domainTree $ strToDomain n

