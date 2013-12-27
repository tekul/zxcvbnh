{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.Tasty.HUnit

import Zxcvbn
import Entropy
import Token

-- Approximate equality for entropy values

(@?~=) :: Double -> Double -> Assertion
(@?~=) actual expected = abs ( actual - expected ) < 1e-5 @? assertion
  where
    assertion = "Expected: " ++ show expected ++ "\nActual: " ++ show actual


main = do
    matchers <- readWordLists
    defaultMain $ tests $ sm : (l33tMatcher matchers) : matchers

sm = sequenceMatcher [lowerCaseAlphabetic, upperCaseAlhabetic, digits]

readWordLists :: IO [Matcher]
readWordLists = do
    passwords <- TIO.readFile "common_passwords_short.txt"
    english   <- TIO.readFile "english.txt"
    return [ dictMatcher "passwords" $ parseDict passwords
           , dictMatcher "english"   $ parseDict english
           ]


tests :: [Matcher] -> TestTree
tests matchers = testGroup "Tests" [zxcvbnTests matchers]


zxcvbnTests :: [Matcher] -> TestTree
zxcvbnTests matchers = testGroup "zxcvbn Tests"
    [ testCase "Entropy of 'correcthorsebatterystaple' == 45.21" $
        ze "correcthorsebatterystaple" @?~= 45.211653
    , testCase "Entropy of 'Amateur' == 9.91" $
        ze "Amateur" @?~= 9.909893
    , testCase "Entropy of 'AmAteUr' == 14.91" $
        ze "AmAteUr" @?~= 14.909893
    , testCase "Entropy of 'bcdef' == 7.022" $
        ze "bcdef" @?~= 7.0223678
    , testCase "Entropy of 'abcdef1234hgfed' == 13.607" $
        ze "abcdef1234hgfed" @?~= 13.60733
    , testCase "Entropy of '4b4cu$' == 13.655" $
        ze "4b4cu$" @?~= 13.65464
    , testCase "Entropy of '48$0|u+10n' == 16.47" $
        ze "48$0|u+10n" @?~= 16.47002
    ]
  where
    ze = fst . (zxcvbn matchers)

repeatTest = map token . repeatMatches . tokenize
prop_RepeatMatchReverse w = repeatTest (T.reverse w) == repeatTest w


-- prop_nCkGEq0 x y = nCk x y >= 0
--
case_nCk0 = 0 @=? nCk 0 1
case_nCk1 = 99884400 @=? nCk 50 7
case_nCk2 = 2505433700 @=? nCk 50 9

prop_UpperCaseEntropyReverse w = extraUpperCaseEntropy (T.reverse w) == extraUpperCaseEntropy w
prop_L33tEntropyReverse w s = extraL33tEntropy (T.reverse w) s == extraL33tEntropy w s

