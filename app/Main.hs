module Main where

import Data.Bool (bool)
import Data.Char (isDigit)
import Data.Text as T (pack)
import Parser as P (completePatterParser, match, partialMatch)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Megaparsec (errorBundlePretty, match, parse)

main :: IO ()
main = do
  args <- getArgs
  let inputPattern = args !! 1
  input <- getLine
  putStr "Parsing pattern:"
  let patt = case parse P.completePatterParser "" $ T.pack inputPattern of
        Left x -> error $ errorBundlePretty x
        Right y -> y

  print patt
  if head args /= "-E"
    then do
      putStrLn "Expected first argument to be '-E'"
      exitFailure
    else do
      either
        ( \e -> do
            putStrLn $ errorBundlePretty e
            exitFailure
        )
        (const exitSuccess)
        $ parse (P.partialMatch P.match patt) ""
        $ T.pack input
