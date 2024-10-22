module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Parser as P (completePatterParser, match)
import Text.Megaparsec (parse,errorBundlePretty, match)
import Data.Bool (bool)

main :: IO ()
main = do
  args <- getArgs
  let pattern = args !! 1
  input <- getLine
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
             (bool exitFailure exitSuccess) 
                $ parse (P.match patt) "" input
