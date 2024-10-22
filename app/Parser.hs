module Parser (match, PatternParser(..), Pattern(..), completePatterParser) where

import Text.Megaparsec (Parsec, many, noneOf, (<?>), choice, try, satisfy, anySingle, token, MonadParsec (eof))
import Text.Megaparsec.Char (string, char)
import Data.Void (Void)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Control.Applicative.Combinators (option)
import qualified Data.Set as Set (empty)

data Pattern = Digit | AlphaNum | Disj [Pattern] | Char Char | Neg [Pattern] | Seq [Pattern]
    deriving Show

type Parser = Parsec Void String

type PatternParser = Parser Pattern
type Matcher = Pattern -> Parser Bool

digitParser :: PatternParser
digitParser = (string "\\d" $> Digit) <?> "digit"

alphaNumParser :: PatternParser
alphaNumParser = (string "\\w" $> AlphaNum) <?> "alphaNum"

charParser :: PatternParser
charParser = (Char <$> noneOf "[]^\\") <?> "singleChar"

completePatterParser :: PatternParser
completePatterParser = Seq <$> (many patternParser <* eof)

patternParser :: PatternParser
patternParser = choice . fmap try $ [digitParser, alphaNumParser, negativeParser, disjointParser, charParser]

disjointParser :: PatternParser
disjointParser = (Disj <$> ((char '[' *> many patternParser) <* char ']')) <?> "choice"

negativeParser :: PatternParser
negativeParser = (Neg <$> ((char '[' *> char '^' *> many patternParser) <* char ']')) <?> "negativeGroup"

match:: Matcher
match Digit = (option False $ satisfy isDigit $> True) <?> "digit"
match AlphaNum = (option False $ satisfy isAlphaNum $> True) <?> "alphaNum"
match (Char c) = (option False $ satisfy (==c) $> True) <?> "char"
match (Disj []) = token (const $ Just False) Set.empty <?> "disjEmpty"
match (Disj (p:ps)) = match p <|> match (Disj ps) <?> "disj"
match (Neg ps) = not <$> match (Disj ps) <?> "negGroup"
match (Seq []) = token (const $ Just False) Set.empty <?> "seqEmpty"
match (Seq [p]) = match p <?> "single p"
match (Seq (p:ps)) = (match p *> match (Seq ps)) <?> "seq"

isAlphaNum :: Char -> Bool
isAlphaNum c = ((c <= 'z') && (c >= 'a')) || isDigit c || ((c <= 'Z') && (c >= 'A')) || (c == '_')
