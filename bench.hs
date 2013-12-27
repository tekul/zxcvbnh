{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Main where

import System.IO
import Zxcvbn
import Criterion.Main
import Control.Parallel.Strategies
import qualified Data.Text.IO as IOT

main = do
    !ms <- defaultMatchers

    defaultMain
      [ bgroup "zxcvbn"
          [ bench "correcthorsebatterystaple" $ nf (zxcvbn ms) "correcthorsebatterystaple"
          ]
      ]

instance NFData Match

defaultMatchers :: IO [Matcher]
defaultMatchers = do
    dictMatchers <- readWordLists
    return $ (l33tMatcher dictMatchers) : theSequenceMatcher : dictMatchers


theSequenceMatcher = sequenceMatcher [lowerCaseAlphabetic, upperCaseAlhabetic, digits]

readWordLists :: IO [Matcher]
readWordLists = do
    passwords <- IOT.readFile "common_passwords_short.txt"
    english   <- IOT.readFile "english.txt"
    return [ dictMatcher "passwords" $ parseDict passwords
           , dictMatcher "english"   $ parseDict english
           ]