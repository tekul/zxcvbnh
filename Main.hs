{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import System.IO
import Zxcvbn

mainLoop :: Dict -> IO ()
mainLoop dict = do
  putStr "zxcvbn> "
  hFlush stdout
  p <- getLine
  if null p
    then return ()
  else do
    putStrLn $ "Matches " ++ ( show $ dictMatches dict p)
    mainLoop dict


main :: IO ()
main = do
  putStrLn $ "Welcome to zxcvbn. Enter a password to check."
  src <- readFile "passwords.txt"
  mainLoop $ parseDict src

