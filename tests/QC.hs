{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module Main where

import Test.QuickCheck
import Test.Framework
import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Zxcvbn
import Entropy


-- prop_nCkGEq0 x y = nCk x y >= 0

case_nCk1 = nCk 50 7 @=? 99884400
case_nCk2 = nCk 50 9 @=? 2505433700

prop_UpperCaseEntropyReverse w = extraUpperCaseEntropy (reverse w) == extraUpperCaseEntropy w
prop_L33tEntropyReverse w s = extraL33tEntropy (reverse w) s == extraL33tEntropy w s

-- runVerbose = $verboseCheckAll

main = $(defaultMainGenerator)

