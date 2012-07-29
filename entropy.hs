{-# LANGUAGE NPlusKPatterns #-}
module Entropy where

import Data.Char
import Data.Int

type P = Int64

nCk :: Int -> Int -> P 
nCk _ 0 = 1
nCk 0 _ = 0
nCk (n+1) (k+1) = (nCk n k) * fromIntegral (n+1) `div` fromIntegral(k+1)

log2 :: P -> Double
log2 = logBase 2 . realToFrac

possibilities :: Int -> Int -> P
possibilities n1 n2 = foldl (\i j -> i + nCk (n1+n2) j ) 0 [0..min n1 n2]


extraUpperCaseEntropy :: String -> Double
extraUpperCaseEntropy s = case countUpperLower s 0 0 of
      (0, _) -> 0
      (1, _) | (isUpper $ head s) || (isUpper $ last s) -> 1
      (_, 0) -> 1
      (nU, nL) -> log2 $ possibilities nU nL

countUpperLower :: String -> Int -> Int -> (Int,Int)
countUpperLower []     nU nL             = (nU,nL)
countUpperLower (c:cs) nU nL | isUpper c = countUpperLower cs (nU+1) nL
                             | isLower c = countUpperLower cs nU   (nL+1)
                             | otherwise = countUpperLower cs nU nL

extraL33tEntropy :: String -> [(Char,Char)] -> Double
extraL33tEntropy word subs = case subPossibilities subs word of
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
        countSubbedUnsubbed sub (c:cs) nS nU  | c == fst sub  = countSubbedUnsubbed sub cs (nS+1) nU
                                              | c == snd sub  = countSubbedUnsubbed sub cs nS (nU+1)
                                              | otherwise     = countSubbedUnsubbed sub cs nS nU

