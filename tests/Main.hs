{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Zxcvbn
import Entropy


prop_L33tEntropyReverse w s = extraL33tEntropy (reverse w) s == extraL33tEntropy w s



main = $quickCheckAll

