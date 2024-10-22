module Parser (partialMatch, match, PatternParser(..), Pattern(..), completePatterParser) where

import Text.Megaparsec (Parsec, many, noneOf, (<?>), choice, try, satisfy, anySingle, token, MonadParsec (eof), manyTill)
import Text.Megaparsec.Char (string, char)
import Data.Void (Void)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Control.Applicative.Combinators (option, skipManyTill)
import qualified Data.Set as Set (empty)

data Pattern = Digit | AlphaNum | Disj [Pattern] | Char Char | Neg [Pattern] | Seq [Pattern]
    deriving Show

type Parser = Parsec Void String

type PatternParser = Parser Pattern
type Matcher = Pattern -> Parser ()

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
disjointParser = (Disj <$> (char '[' *> many patternParser <* char ']')) <?> "choice"

negativeParser :: PatternParser
negativeParser = (Neg <$> (char '[' *> char '^' *> manyTill patternParser (char ']'))) <?> "negativeGroup"

match:: Matcher
match Digit = satisfy isDigit $> () <?> "digit"
match AlphaNum = (satisfy isAlphaNum $> ()) <?> "alphaNum"
match (Char c) = (satisfy (==c) $> ()) <?> "char"
match (Disj []) = error "this cannot be" <?> "disjEmpty"
match (Disj (p:ps)) = match p <|> match (Disj ps) <?> "disj"
match (Neg []) = token (const $ Just ()) Set.empty <?> "negGroupEmpty"
match (Neg (p:ps)) = (try (match p) *> error "This is an error") <|> match (Neg ps) <?> "negGroup"
match (Seq []) = eof <?> "seqEmpty"
match (Seq [p]) = match p <?> "single p"
match (Seq (p:ps)) = (match p *> match (Seq ps)) <?> "seq"


partialMatch :: Matcher -> Matcher
partialMatch match p = skipManyTill anySingle (try $ match p) <* many anySingle <* eof

isAlphaNum :: Char -> Bool
isAlphaNum c = (c <= 'z') && (c >= 'a') || isDigit c || (c <= 'Z') && (c >= 'A') || c == '_'
