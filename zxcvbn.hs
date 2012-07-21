{-# LANGUAGE DoAndIfThenElse #-}

module Zxcvbn where

import qualified Data.Map as M
import System.Exit
import qualified Data.List as L
import Data.Maybe (mapMaybe)

log2 = logBase 2


type Password = String
type Dict = M.Map String  Int

lookup d t@(Token w _ _) = case M.lookup w d of
  Just r -> Just $ DictMatch t "somename" w r
  _      -> Nothing

class Match a where
  entropy :: a -> Double
  pattern :: a -> String


data Token = Token String Int Int -- Match within a password and its start and end character
                deriving Show

data DictMatch = DictMatch Token String String Int --  token, dictname, matched word, rank
                deriving Show

data L33tMatch = L33tMatch DictMatch [(Char,Char)] --  token, dictname, unl33ted, rank, subs

rank (DictMatch _ _ _ r) = r

instance Match DictMatch where
  pattern _ = "dictionary"
  entropy = log2 . fromIntegral . rank

instance Match L33tMatch where
  pattern (L33tMatch d _) = pattern d
  entropy (L33tMatch d@(DictMatch t _ _ _) _) = entropy d 

parseDict :: String -> Dict
parseDict src = foldl (\m (k,v) -> M.insert k v m) M.empty elts
  where words = lines src
        elts  = zip words [1..length words]


dictMatches :: Dict -> String -> [DictMatch]
dictMatches dict password = dictMatch dict [] (tokenize password)

dictMatch :: Dict -> [DictMatch] -> [Token] -> [DictMatch]
dictMatch d matches [] = matches 
dictMatch d matches ts = mapMaybe (\t -> Zxcvbn.lookup d t) ts

continuousSubSeqs = filter (not . null) . concatMap L.tails . L.inits

tokenize xs = map (\(s, l) -> Token s (head l) (last l)) $ zip s ind
    where s = continuousSubSeqs xs
          ind = continuousSubSeqs [0..]

  
