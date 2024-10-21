module Parser (match, PatternParser(..), Pattern(..), patternParser) where

import Text.Megaparsec (Parsec, many, noneOf, (<?>), choice)
import Text.Megaparsec.Char (string, char)
import Data.Void (Void)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Char (isDigit)

data Pattern = Digit | AlphaNum | Disj [Pattern] | Char Char

type Parser = Parsec Void String

type PatternParser = Parser Pattern

digitParser :: PatternParser
digitParser = (string "\\d" $> Digit) <?> "digit"

alphaNumParser :: PatternParser
alphaNumParser = (string "\\w" $> AlphaNum) <?> "alphaNum"

charParser :: PatternParser
charParser = (Char <$> noneOf "[]\\") <?> "singleChar"

patternParser :: PatternParser
patternParser = choice [digitParser, alphaNumParser, disjointParser, charParser]

disjointParser :: PatternParser
disjointParser = (Disj <$> ((char '[' *> many patternParser) <* char ']')) <?> "choice"

match:: Pattern -> String -> Bool
match Digit input = any isDigit input
match AlphaNum input = any isAlphaNum input
match (Char c) input = c `elem` input
match (Disj []) _ = False
match (Disj [p]) input = match p input
match (Disj (p:ps)) input = match p input || match (Disj ps) input

isAlphaNum :: Char -> Bool
isAlphaNum c = ((c <= 'z') && (c >= 'a')) || isDigit c || ((c <= 'Z') && (c >= 'A')) || (c == '_')
