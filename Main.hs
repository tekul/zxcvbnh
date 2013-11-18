{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import System.IO
import Control.Monad (unless)
import Zxcvbn

mainLoop :: [Matcher] -> IO ()
mainLoop matchers = do
  putStr "zxcvbn> "
  hFlush stdout
  p <- getLine
  unless (null p) $ do
    putStrLn $ "Matches " ++ show (zxcvbn p matchers)
    mainLoop matchers


main :: IO ()
main = do
  putStrLn "Welcome to zxcvbn. Enter a password to check."
  src <- readFile "passwords.txt"
  mainLoop [dictMatcher "passwords" $ parseDict src]

