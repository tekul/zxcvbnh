{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.DeepSeq
import Data.Text (Text)
import Data.Map.Strict (Map)
import GHC.Generics

type Password = Text
type Dict = Map Text Int

-- | A token within a password and its start and end character
data Token = Token !Text !Int !Int deriving (Show, Eq, Generic)

-- | A match containing the token, entropy and type of the match
data Match = Match !Token !Double !MatchType deriving (Show, Eq, Generic)

data MatchType
    = DictMatch String -- dictname
    | L33tMatch String String [(Char,Char)] --  dictname, unl33ted, subs
    | SequenceMatch
    | RepeatMatch Char
    | BruteForceMatch
    | SpatialMatch String
    | DigitsMatch
    | YearMatch
    | DateMatch Int Bool
    deriving (Show, Eq, Generic)

instance NFData Token
instance NFData Match
instance NFData MatchType

type Matcher = Password -> [Match]

instance Ord Token where
  compare (Token _ i j) (Token _ k l)
    | i <= k && j <= l = LT
    | i == k && j <= l = LT
    | i == k && j == l = EQ
    | otherwise        = GT
