{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeSynonymInstances #-}

module Network.ZDNS.Types.Name where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import qualified Data.Vector as V
import qualified Data.List as L
import Network.ZDNS.Util

----------------------------------------------------------------
-- | type label
data DomainRelation = SUPERDOMAIN 
                    | SUBDOMAIN 
                    | EQUALDOMAIN 
                    | COMMONANCESTOR 
                    | NORELATION deriving (Eq, Show)

maxLabelCount :: Int
maxLabelCount = 127

maxLabelLen :: Int
maxLabelLen = 63

maxDomainLen :: Int
maxDomainLen = 255

data Label = Label {
    labelData   :: BC.ByteString
 ,  labelLen    :: Int
} 

emptyLabel :: Label 
emptyLabel = Label BS.empty 1

downcaseLabel :: Label -> Label
downcaseLabel (Label d len)  = Label (BC.map toLower d) len

instance Eq Label where
    (==) (Label d1 l1) (Label d2 l2) 
            | l1 /= l2 = False
            | otherwise = and $ BC.zipWith charIsEqal d1 d2
            where charIsEqal c1 c2 = toLower c1 == toLower c2
    
mkLabel :: String -> Maybe Label
mkLabel s  = mkLabel' $ BC.pack s

mkLabel' :: BS.ByteString -> Maybe Label
mkLabel' s | BS.null s = Nothing
           | otherwise = if BS.length s > maxLabelLen
                        then Nothing
                        else Just $ Label s (BS.length s)

instance Ord Label where
    compare (Label d1 l1) (Label d2 l2) = 
        case firstNoneEqResult of
            Nothing -> compare l1 l2
            Just ret -> ret
        where compareChar (c1, c2) = compare (toLower c1) (toLower c2)
              firstNoneEqResult = L.find (\r -> r /= EQ) $ map compareChar $ BC.zip d1 d2 
        
instance Show Label where 
    show = BC.unpack . labelData 

----------------------------------------------------------------
-- | type Domain
type LabelVec = V.Vector Label
data Domain = Domain {
    labels :: LabelVec
 ,  domainLength :: Int
} 

labelCount :: Domain -> Int
labelCount (Domain ls _) = V.length ls

mkDomain :: String -> Maybe Domain
mkDomain s = case mkLabels s (V.fromList []) of
                Nothing -> Nothing
                Just ls -> mkDomain' ls

mkDomain' :: LabelVec -> Maybe Domain
mkDomain' labels = let domainLen = V.sum $ V.map labelLen labels
                       lc = V.length labels
                        in if or [domainLen > maxDomainLen
                                , lc > maxLabelCount
                                , lc == 0]
                            then Nothing
                            else Just $ Domain labels domainLen 

mkLabels :: String -> LabelVec -> Maybe LabelVec
mkLabels s labels = if L.null ls
                    then Just $ V.snoc labels emptyLabel
                    else
                        case mkLabel ls of
                            Nothing -> Nothing
                            Just l -> mkLabels (L.drop 1 left) (V.snoc labels l)
                     where (ls, left) = L.break (\c -> c == '.') s 


instance Show Domain where 
    show n = join "." (V.toList . V.map show $ labels n)

instance Eq Domain where
    (==) n1@(Domain labels1 len1) n2@(Domain labels2 len2) 
        | len1 /= len2 = False
        | labelCount n1 /= labelCount n2 = False
        | otherwise = labels1 == labels2

instance Ord Domain where
    compare n1@(Domain labels1 _) n2@(Domain labels2 _) = 
        if labelCompResult /= EQ
        then labelCompResult
        else compare (labelCount n1) (labelCount n2) 
        where 
            compareLabelCount = min (labelCount n1) (labelCount n2) 
            labelsToComp ls = V.take compareLabelCount $ (V.tail . V.reverse) ls
            labelCompResult = compare (labelsToComp labels1) (labelsToComp labels2) 

data DomainCompResult = DCR { 
    ord :: Ordering
 ,  commonLable :: Int
 ,  relation :: DomainRelation
} deriving (Eq, Show)

compareDomain :: Domain -> Domain -> DomainCompResult
compareDomain n1@(Domain labels1 _) n2@(Domain labels2 _) = 
    case firstUnEqIndex of
        Just index -> if index == 0 
                      then DCR (labelCompResults V.! index) 1 NORELATION
                      else DCR (labelCompResults V.! index) (index + 1) COMMONANCESTOR
        Nothing -> case compare lc1 lc2 of
                        EQ -> DCR EQ lc1 EQUALDOMAIN
                        LT -> DCR LT lc1 SUPERDOMAIN
                        GT -> DCR GT lc2 SUBDOMAIN
    where 
        (lc1, lc2) = (labelCount n1, labelCount n2)
        compareLabelCount = min lc1 lc2
        labelsToComp ls = V.take compareLabelCount $ (V.tail . V.reverse) ls
        labelCompResults = V.zipWith compare (labelsToComp labels1) (labelsToComp labels2) 
        firstUnEqIndex = V.findIndex (\r -> r /= EQ) labelCompResults

splitDomain :: Int -> Int -> Domain -> Maybe Domain
splitDomain start count name 
    | or [start < 0, count < 0, start + count > labelCount name] = Nothing
    | start + count == labelCount name = mkDomain' $ V.slice start count (labels name)
    | otherwise = mkDomain' $ V.snoc (V.slice start count (labels name)) emptyLabel

superDomain :: Int -> Domain -> Maybe Domain
superDomain level name 
    | or [level < 0, level >= labelCount name] = Nothing
    | level == 0 = Just name
    | (level + 1) == labelCount name = Just rootDomain
    | otherwise = mkDomain' $ V.drop level (labels name)

concatDomain :: Domain -> Domain -> Maybe Domain
concatDomain n1 n2 = mkDomain' $ (V.init $ labels n1) V.++ (labels n2)

downcaseDomain :: Domain -> Domain
downcaseDomain (Domain labels len) =
    Domain (V.map downcaseLabel labels) len

rootDomain :: Domain
rootDomain = let Just n = mkDomain' $ V.fromList [emptyLabel]
                in n

isRoot :: Domain -> Bool
isRoot d = d == rootDomain
