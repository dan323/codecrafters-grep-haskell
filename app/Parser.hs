module Parser (partialMatch, match, PatternParser(..), Pattern(..), completePatterParser) where

import Text.Megaparsec (Parsec, many, noneOf, (<?>), choice, try, satisfy, anySingle, token, MonadParsec (eof), manyTill, getParserState, stateOffset, oneOf)
import Text.Megaparsec.Char (string, char)
import Data.Void (Void)
import Data.Functor (($>))
import Control.Applicative ((<|>), (<**>))
import Data.Char (isDigit)
import Control.Applicative.Combinators (option, skipManyTill, optional)
import qualified Data.Set as Set (empty)

data Pattern = Digit     -- "\d"
    | AlphaNum           -- "\w"
    | Disj [Pattern]     -- "[...]" or "(...|...)"
    | Char Char          -- "a" 
    | Neg [Char]         -- "[^...]"
    | Seq [Pattern]      -- juxtaposition
    | Start              -- "^"
    | End                -- "$"
    | OneOrMore Pattern  -- "+"
    | ZeroOrMore Pattern -- "*"
    | Optional Pattern   -- "?"
    | Group [Pattern]    -- "(...)"
    | Wildcard           -- "."
    deriving Show

type Parser = Parsec Void String

type PatternParser = Parser Pattern
type Matcher = Pattern -> Parser ()

reservedChars :: String
reservedChars = "[]^\\$+*()?.|/"

digitParser :: PatternParser
digitParser = (string "\\d" $> Digit) <?> "digit"

alphaNumParser :: PatternParser
alphaNumParser = (string "\\w" $> AlphaNum) <?> "alphaNum"

scapedCharParser :: PatternParser
scapedCharParser = Char <$> (char '\\' *> oneOf reservedChars)

regularCharParser :: PatternParser
regularCharParser = (Char <$> noneOf reservedChars) <?> "singleChar"

charParser :: PatternParser
charParser = try scapedCharParser <|> regularCharParser

startParser :: PatternParser
startParser = (char '^' $> Start) <?> "start"

endParser :: PatternParser
endParser = (char '$' $> End) <?> "end"

wildcardParser :: PatternParser
wildcardParser = (char '.' $> Wildcard) <?> "wildcard"

optionalParser :: PatternParser
optionalParser = patternParser >>= (\patt -> char '?' $> Optional patt) <?> "zeroOrOne"

oneOrMoreParser :: PatternParser
oneOrMoreParser = patternParser >>= (\patt -> char '+' $> OneOrMore patt) <?> "oneOrMore"

zeroOrMoreParser :: PatternParser
zeroOrMoreParser = patternParser >>= (\patt -> char '*' $> ZeroOrMore patt) <?> "zeroOrMore"

completePatterParser :: PatternParser
completePatterParser = Seq <$> ((optional startParser >>= maybe (pure id) (const $ pure (Start :))) <*> ((many patternWithRepetitionParser <**> (optional endParser >>= maybe (pure id) (const $ pure (++ [End])))) <* eof))

patternParser :: PatternParser
patternParser = choice . fmap try $ [digitParser, alphaNumParser, negativeParser, disjointCharParser, disjointPatternParser, groupParser, wildcardParser, charParser]

patternWithRepetitionParser :: PatternParser
patternWithRepetitionParser = choice . fmap try $ [ oneOrMoreParser, zeroOrMoreParser, optionalParser, patternParser]

groupParser :: PatternParser
groupParser = (Group <$> (char '(' *> manyTill patternWithRepetitionParser (char ')'))) <?> "grouped"

disjointCharParser :: PatternParser
disjointCharParser = (Disj <$> (char '[' *> manyTill charParser (char ']'))) <?> "choice"

disjointPatternParser :: PatternParser
disjointPatternParser = (Disj <$> (char '(' *> ((:) <$> (Seq <$> many patternWithRepetitionParser) <*> manyTill (char '|' *> (Seq <$> many patternWithRepetitionParser)) (char ')')))) <?> "disjoint"

negativeParser :: PatternParser
negativeParser = (Neg <$> (string "[^" *> manyTill (charParser >>= returnChar) (char ']'))) <?> "negativeGroup"
    where
        returnChar t = case t of
            Char c -> pure c
            _      -> error "Char pattern expected"

match:: Matcher
match Digit = satisfy isDigit $> () <?> "digit"
match AlphaNum = (satisfy isAlphaNum $> ()) <?> "alphaNum"
match (Char c) = (satisfy (==c) $> ()) <?> "char"
match (Disj []) = error "this cannot be" <?> "disjEmpty"
match (Disj (p:ps)) = match p <|> match (Disj ps) <?> "disj"
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
match Wildcard = anySingle $> () <?> "wildcard"
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
