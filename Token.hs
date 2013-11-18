module Token where

import Data.List as L

data Token = Token String Int Int -- Match within a password and its start and end character
                deriving (Show, Eq)

subtoken :: Token -> Token -> Bool
subtoken (Token _ i j) (Token _ k l) = i >= k && j <= l

token (Token t _ _) = t

instance Ord Token where
  compare (Token _ i j) (Token _ k l)
    | i <= k && j <= l = LT
    | i == k && j <= l = LT
    | i == k && j == l = EQ
    | otherwise        = GT

-- Creates a list of all possible tokens of a string
tokenize :: String -> [Token]
tokenize xs = L.sort $ map (\(s, l) -> Token s (head l) (last l)) $ zip ss ind
  where
    ss  = continuousSubSeqs xs
    ind = continuousSubSeqs [0..]
    continuousSubSeqs = filter (not . null) . concatMap L.tails . L.inits


-- Removes tokens from a list which are subtokens of another token in the list
removeSubTokens :: [Token] -> [Token]
removeSubTokens [] = []
removeSubTokens [t] = [t]
removeSubTokens (t:ts)
    | t `subtoken` head ts = removeSubTokens ts
    | head ts `subtoken` t = removeSubTokens $ t:tail ts
    | otherwise            = t : removeSubTokens ts


