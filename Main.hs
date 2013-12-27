{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
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

doZxcvbn :: [Matcher] -> [String] -> IO ()
doZxcvbn ms []     = return ()
doZxcvbn ms (p:ps) = (putStrLn $ show (zxcvbn ms p)) >> doZxcvbn ms ps


main :: IO ()
main = do
    args <- getArgs
    ms   <- defaultMatchers
    case args of
      [] -> putStrLn "Welcome to zxcvbn. Enter a password to check." >>  mainLoop ms
      _  -> (readFile $ head args) >>= return . lines >>= doZxcvbn ms

defaultMatchers :: IO [Matcher]
defaultMatchers = do
    dictMatchers <- readWordLists
    return $ (l33tMatcher dictMatchers) : theSequenceMatcher : dictMatchers

theSequenceMatcher = sequenceMatcher [lowerCaseAlphabetic, upperCaseAlhabetic, digits]

readWordLists :: IO [Matcher]
readWordLists = do
    passwords <- readFile "common_passwords_short.txt"
    english   <- readFile "english.txt"
    return [ dictMatcher "passwords" $ parseDict passwords
           , dictMatcher "english"   $ parseDict english
           ]
