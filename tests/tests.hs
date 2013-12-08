{-# OPTIONS_GHC -w #-}

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
    defaultMain $ tests matchers

readWordLists :: IO [Matcher]
readWordLists = do
    passwords <- readFile "common_passwords_short.txt"
    english   <- readFile "english.txt"
    return [ dictMatcher "passwords" $ parseDict passwords
           , dictMatcher "english"   $ parseDict english
           ]


tests :: [Matcher] -> TestTree
tests matchers = testGroup "Tests" [sequenceTests, zxcvbnTests matchers]


zxcvbnTests :: [Matcher] -> TestTree
zxcvbnTests matchers = testGroup "zxcvbn Tests"
    [ testCase "Entropy of 'correcthorsebatterystaple' == 45.21" $
        ze "correcthorsebatterystaple" @?~= 45.211653
    , testCase "Entropy of 'Amateur' == 9.91" $
        ze "Amateur" @?~= 9.909893
    , testCase "Entropy of 'AmAteUr' == 14.91" $
        ze "AmAteUr" @?~= 14.909893
    ]
  where
    ze = fst . (zxcvbn matchers)

repeatTest = map token . repeatMatches . tokenize
prop_RepeatMatchReverse w = repeatTest (reverse w) == repeatTest w

-- Sequence tests

sequenceTests :: TestTree
sequenceTests = testGroup "Sequence Tests" [sequenceMatchUnitTests, candidateSequenceUnitTests]

sequenceMatchUnitTests = testGroup "sequenceMatch Unit Tests"
    [ testCase "ab matches lower case" $
        smlc 'a' 'b' @?= Just 1
    , testCase "ba matches lower case reverse" $
        smlc 'b' 'a' @?= Just (-1)
    , testCase "01 does not match alphabetic" $
        smlc '0' '1' @?= Nothing
    , testCase "cd matches lower case" $
        smlc 'c' 'd' @?= Just 1
    , testCase "yz matches lower case" $
        smlc 'y' 'z' @?= Just 1
    , testCase "zy matches lower case reverse" $
        smlc 'z' 'y' @?= Just (-1)
    ]
  where
    smlc = sequenceMatch lowerCaseAlphabetic

candidateSequenceUnitTests = testGroup "candidateSequence Unit Tests"
    [ testCase "Candidate for AB is upper case forward" $
        candidateSequence 'A' 'B' sequences @?= Just (upperCaseAlhabetic, 1)
    , testCase "Candidate for 'ak' is Nothing" $
        candidateSequence 'a' 'k' sequences @?= Nothing
    , testCase "Candidate for 10 is digits reverse" $
        candidateSequence '1' '0' sequences @?= Just (digits, -1)
    ]

-- prop_nCkGEq0 x y = nCk x y >= 0
--
case_nCk0 = 0 @=? nCk 0 1
case_nCk1 = 99884400 @=? nCk 50 7
case_nCk2 = 2505433700 @=? nCk 50 9

prop_UpperCaseEntropyReverse w = extraUpperCaseEntropy (reverse w) == extraUpperCaseEntropy w
prop_L33tEntropyReverse w s = extraL33tEntropy (reverse w) s == extraL33tEntropy w s

