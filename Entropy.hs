{-# LANGUAGE NPlusKPatterns #-}
module Entropy
  ( bruteForceCardinality
  , extraUpperCaseEntropy
  , extraL33tEntropy
  , log2
  , nCk
  )
where

import Data.Char
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T

type P = Int64

nCk :: Int -> Int -> P
nCk _ 0 = 1
nCk 0 _ = 0
nCk (n+1) (k+1) = (nCk n k) * fromIntegral (n+1) `div` fromIntegral(k+1)

log2 :: P -> Double
log2 = logBase 2 . realToFrac

possibilities :: Int -> Int -> P
possibilities n1 n2 = foldl (\i j -> i + nCk (n1+n2) j ) 0 [0..min n1 n2]

bruteForceCardinality :: Text -> Int
bruteForceCardinality p = upper + lower + digits + symbols
  where
    (upper, lower, digits, symbols) = T.foldl' cardinality (0,0,0,0) p
    cardinality (u,l,d,s) c
      | isLower c = (u,26,d,s)
      | isDigit c = (u,l,10,s)
      | isUpper c = (26,l,d,s)
      | otherwise = (u,l,d,33)

extraUpperCaseEntropy :: Text -> Double
extraUpperCaseEntropy s = case countUpperLower s of
    (0, _) -> 0
    (1, _) | (isUpper $ T.head s) || (isUpper $ T.last s) -> 1
    (_, 0) -> 1
    (nU, nL) -> log2 $ possibilities nU nL

countUpperLower s = T.foldl' ul (0,0) s
  where
    ul (nU,nL) c
        | isUpper c = (nU+1, nL)
        | isLower c = (nU, nL+1)
        | otherwise = (nU, nL)

extraL33tEntropy :: Text -> [(Char,Char)] -> Double
extraL33tEntropy word subs
  | T.null word || null subs = 0
  | otherwise = case subPossibilities subs (T.unpack word) of
                  1 -> 1.0
                  p -> log2 $ fromIntegral p
    where
      subPossibilities :: [(Char,Char)] -> String -> P
      subPossibilities [] _     = 0
      subPossibilities (s:ss) w = possibilities nS nU + subPossibilities ss w
        where
          (nS, nU) = countSubbedUnsubbed s w 0 0

      countSubbedUnsubbed :: (Char,Char) -> String -> Int -> Int -> (Int,Int)
      countSubbedUnsubbed _ [] nS nU       = (nS,nU)
      countSubbedUnsubbed sub (c:cs) nS nU
          | c == fst sub  = countSubbedUnsubbed sub cs (nS+1) nU
          | c == snd sub  = countSubbedUnsubbed sub cs nS (nU+1)
          | otherwise     = countSubbedUnsubbed sub cs nS nU
