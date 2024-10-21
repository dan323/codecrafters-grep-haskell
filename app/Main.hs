module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Parser as P (patternParser, match)
import Text.Megaparsec (parse,errorBundlePretty, match)

main :: IO ()
main = do
  args <- getArgs
  let pattern = args !! 1
  input <- getLine
  let patt = case parse P.patternParser "" pattern of
              Left x -> error $ errorBundlePretty x
              Right y -> y

  if head args /= "-E"
    then do
      putStrLn "Expected first argument to be '-E'"
      exitFailure
    else do
      if P.match patt input
        then exitSuccess
        else exitFailure
