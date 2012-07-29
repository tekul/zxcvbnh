module Zxcvbn where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Entropy


type Password = String
type Dict = M.Map String  Int

lookup d t@(Token w _ _) = case M.lookup w d of
  Just r -> Just $ DictMatch t "somename" w r
  _      -> Nothing


data Token = Token String Int Int -- Match within a password and its start and end character
                deriving Show

data Match = DictMatch Token String String Int --  token, dictname, matched word, rank
           | L33tMatch Token String String Int [(Char,Char)] --  token, dictname, unl33ted, rank, subs
            deriving Show

pattern :: Match -> String
pattern DictMatch {} = "dictionary"
pattern L33tMatch {} = "dictionary"

entropy :: Match -> Double
entropy (DictMatch (Token s _ _) _ _ rank) = log2 (fromIntegral rank) + extraUpperCaseEntropy s
entropy (L33tMatch (Token s _ _) _ _ rank subs) = log2 (fromIntegral rank) + extraUpperCaseEntropy s + extraL33tEntropy s subs


parseDict :: String -> Dict
parseDict src = foldl (\m (k,v) -> M.insert k v m) M.empty elts
  where words = lines src
        elts  = zip words [1..length words]


dictMatches :: Dict -> String -> [Match]
dictMatches dict password = dictMatch dict [] (tokenize password)

dictMatch :: Dict -> [Match] -> [Token] -> [Match]
dictMatch _ matches [] = matches
dictMatch d matches ts = mapMaybe (Zxcvbn.lookup d) ts

continuousSubSeqs = filter (not . null) . concatMap L.tails . L.inits

tokenize xs = map (\(s, l) -> Token s (head l) (last l)) $ zip s ind
    where s = continuousSubSeqs xs
          ind = continuousSubSeqs [0..]

