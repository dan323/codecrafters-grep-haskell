module Parser (partialMatch, match, PatternParser(..), Pattern(..), completePatterParser) where

import Text.Megaparsec (Parsec, many, noneOf, (<?>), choice, try, satisfy, anySingle, token, MonadParsec (eof), manyTill, getParserState, stateOffset, oneOf)
import Text.Megaparsec.Char (string, char)
import Data.Void (Void)
import Data.Functor (($>))
import Control.Applicative ((<|>), (<**>))
import Data.Char (isDigit)
import Control.Applicative.Combinators (option, skipManyTill, optional)
import qualified Data.Set as Set (empty)
import System.Console.GetOpt (OptDescr(Option))

data Pattern = Digit     -- "\d"
    | AlphaNum           -- "\w"
    | Disj [Char]     -- "[...]"
    | Char Char          -- "a" 
    | Neg [Char]      -- "[^...]"
    | Seq [Pattern]      -- juxtaposition
    | Start              -- "^"
    | End                -- "$"
    | OneOrMore Pattern  -- "+"
    | ZeroOrMore Pattern -- "*"
    | Optional Pattern   -- "?"
    | Group [Pattern]    -- "(...)"
    deriving Show

type Parser = Parsec Void String

type PatternParser = Parser Pattern
type Matcher = Pattern -> Parser ()

reservedChars :: String
reservedChars = "[]^\\$+*()?"

digitParser :: PatternParser
digitParser = (string "\\d" $> Digit) <?> "digit"

alphaNumParser :: PatternParser
alphaNumParser = (string "\\w" $> AlphaNum) <?> "alphaNum"

scapedCharParser :: PatternParser
scapedCharParser = Char <$> (char '\\' *> oneOf reservedChars)

charParser :: PatternParser
charParser = (Char <$> noneOf reservedChars) <?> "singleChar"

startParser :: PatternParser
startParser = (char '^' $> Start) <?> "start"

endParser :: PatternParser
endParser = (char '$' $> End) <?> "end"

optionalParser :: PatternParser
optionalParser = patternParser >>= (\patt -> char '?' $> Optional patt) <?> "zeroOrOne"

oneOrMoreParser :: PatternParser
oneOrMoreParser = patternParser >>= (\patt -> char '+' $> OneOrMore patt) <?> "oneOrMore"

zeroOrMoreParser :: PatternParser
zeroOrMoreParser = patternParser >>= (\patt -> char '*' $> ZeroOrMore patt) <?> "zeroOrMore"

completePatterParser :: PatternParser
completePatterParser = Seq <$> ((optional startParser >>= maybe (pure id) (const $ pure (Start :))) <*> ((many patternWithRepetitionParser <**> (optional endParser >>= maybe (pure id) (const $ pure (++ [End])))) <* eof))

patternParser :: PatternParser
patternParser = choice . fmap try $ [digitParser, alphaNumParser, negativeParser, disjointParser, groupParser, charParser]

patternWithRepetitionParser :: PatternParser
patternWithRepetitionParser = choice . fmap try $ [ oneOrMoreParser, zeroOrMoreParser, optionalParser, patternParser]

groupParser :: PatternParser
groupParser = (Group <$> (char '(' *> manyTill patternWithRepetitionParser (char ')'))) <?> "choice"

disjointParser :: PatternParser
disjointParser = (Disj <$> (char '[' *> manyTill ((try scapedCharParser <|> charParser) >>= (\(Char c) -> pure c)) (char ']'))) <?> "choice"

negativeParser :: PatternParser
negativeParser = (Neg <$> (string "[^" *> manyTill ((try scapedCharParser <|> charParser) >>= (\(Char c) -> pure c)) (char ']'))) <?> "negativeGroup"

match:: Matcher
match Digit = satisfy isDigit $> () <?> "digit"
match AlphaNum = (satisfy isAlphaNum $> ()) <?> "alphaNum"
match (Char c) = (satisfy (==c) $> ()) <?> "char"
match (Disj []) = error "this cannot be" <?> "disjEmpty"
match (Disj (p:ps)) = match (Char p) <|> match (Disj ps) <?> "disj"
match (Neg []) = (anySingle $> ()) <?> "negGroupEmpty"
match (Neg (p:ps)) = try (match (Char p) *> error "This is an error") <|> match (Neg ps) <?> "negGroup"
match (Seq []) = pure () <?> "seqEmpty"
match (Seq (p:ps)) = (match p *> match (Seq ps)) <?> "seq"
match Start = do
    state <- getParserState
    let processed = stateOffset state
    if processed == 0
        then pure ()
        else error "No match"
match End = eof
match (Optional p) = optional (match p) $> () <?> "optional"
match (OneOrMore p) = match p *> match (ZeroOrMore p)
match (ZeroOrMore p) = do
        may <- optional (match p)
        case may of
            Just () -> match (ZeroOrMore p)
            Nothing -> pure ()


partialMatch :: Matcher -> Matcher
partialMatch match p = skipManyTill anySingle (try $ match p) <* many anySingle <* eof

isAlphaNum :: Char -> Bool
isAlphaNum c = (c <= 'z') && (c >= 'a') || isDigit c || (c <= 'Z') && (c >= 'A') || c == '_'
