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
      putStrLn $ "Matches: " ++ show (zxcvbn matchers p)
      mainLoop matchers


main :: IO ()
main = putStrLn "Welcome to zxcvbn. Enter a password to check." >> readWordLists >>= mainLoop

readWordLists :: IO [Matcher]
readWordLists = do
    passwords <- readFile "common_passwords_short.txt"
    english   <- readFile "english.txt"
    return [ dictMatcher "passwords" $ parseDict passwords
           , dictMatcher "english"   $ parseDict english
           ]
