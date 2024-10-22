module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Parser as P (completePatterParser, partialMatch, match, partialMatch)
import Text.Megaparsec (parse, errorBundlePretty, match)
import Data.Bool (bool)

main :: IO ()
main = do
  args <- getArgs
  let pattern = args !! 1
  input <- getLine
  putStr "Parsing pattern:"
  let patt = case parse P.completePatterParser "" pattern of
              Left x -> error $ errorBundlePretty x
              Right y -> y

  print patt
  if head args /= "-E"
    then do
      putStrLn "Expected first argument to be '-E'"
      exitFailure
    else do
      either (const exitFailure)
             (const exitSuccess)
                $ parse (P.partialMatch P.match patt) "" input
