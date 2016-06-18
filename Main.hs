{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.IO
import Control.Monad (unless)
import Zxcvbn
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

mainLoop :: [Matcher] -> IO ()
mainLoop matchers = do
    putStr "zxcvbn> "
    hFlush stdout
    p <- TIO.getLine
    unless (T.null p) $ do
      putStrLn $ "Matches: " ++ show (zxcvbn matchers p)
      mainLoop matchers

doZxcvbn :: [Matcher] -> [T.Text] -> IO ()
doZxcvbn ms []     = return ()
doZxcvbn ms (p:ps) = (putStrLn $ show (zxcvbn ms p)) >> doZxcvbn ms ps

main :: IO ()
main = do
    args <- getArgs
    ms   <- defaultMatchers
    case args of
      []    -> putStrLn "Welcome to zxcvbn. Enter a password to check." >>  mainLoop ms
      (f:_) -> do
          passwords <- fmap T.lines (TIO.readFile f)
          doZxcvbn ms passwords

defaultMatchers :: IO [Matcher]
defaultMatchers = do
    dictMatchers <- readWordLists
    return $ (l33tMatcher dictMatchers) : theSequenceMatcher : dictMatchers

theSequenceMatcher = sequenceMatcher [lowerCaseAlphabetic, upperCaseAlphabetic, digits]

readWordLists :: IO [Matcher]
readWordLists = do
    passwords <- TIO.readFile "common_passwords_short.txt"
    english   <- TIO.readFile "english.txt"
    return [ dictMatcher "passwords" $ T.lines passwords
           , dictMatcher "english"   $ T.lines english
           ]
