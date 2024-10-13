module Main where

import System.Environment
import System.Exit
import Data.Char(isDigit)

matchPattern :: String -> String -> Bool
matchPattern pattern input = case pattern of
  "\\d" -> any isDigit input
  "\\w" -> any isAlphaNum input
  [c] -> head pattern `elem` input
  otherwise -> error $ "Unhandled pattern: " ++ pattern

isAlphaNum :: Char -> Bool
isAlphaNum c = ((c <= 'z') && (c >= 'a')) || ((c >= '0') && (c <= '9')) || ((c <= 'Z') && (c >= 'A')) || (c == '_')

main :: IO ()
main = do
  args <- getArgs
  let pattern = args !! 1
  input_line <- getLine

  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  putStrLn "Logs from your program will appear here"

  -- Uncomment this block to pass stage 1
  if head args /= "-E"
   then do
     putStrLn "Expected first argument to be '-E'"
     exitFailure
   else do if matchPattern pattern input_line
            then exitSuccess
            else exitFailure
