{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Zxcvbn
import Criterion.Main
import qualified Data.Text.IO as IOT

main = do
    !ms <- defaultMatchers

    defaultMain
      [ bgroup "zxcvbn"
          [ bench "correcthorsebatterystaple" $ nf (zxcvbn ms) "correcthorsebatterystaple"
          ]
      ]

defaultMatchers :: IO [Matcher]
defaultMatchers = do
    dictMatchers <- readWordLists
    return $ l33tMatcher dictMatchers : theSequenceMatcher : dictMatchers


theSequenceMatcher = sequenceMatcher [lowerCaseAlphabetic, upperCaseAlphabetic, digits]

readWordLists :: IO [Matcher]
readWordLists = do
    passwords <- IOT.readFile "common_passwords_short.txt"
    english   <- IOT.readFile "english.txt"
    return [ dictMatcher "passwords" $ T.lines passwords
           , dictMatcher "english"   $ T.lines english
           ]
