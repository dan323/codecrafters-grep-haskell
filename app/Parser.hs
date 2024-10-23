{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser (partialMatch, match, PatternParser(..), Pattern(..), completePatterParser) where

import Text.Megaparsec (Parsec, many, noneOf, (<?>), choice, try, satisfy, anySingle, token, MonadParsec (eof), manyTill, getParserState, stateOffset, oneOf, stateInput)
import Text.Megaparsec.Char (string, char)
import Data.Text as T (Text, take, unpack)
import Data.Void (Void)
import Data.Functor (($>))
import Control.Applicative ((<|>), (<**>))
import Data.Char (isDigit)
import Control.Applicative.Combinators (option, skipManyTill, optional)
import qualified Data.Set as Set (empty)
import Data.List as L (singleton)

data Pattern = Digit     -- "\d"
    | AlphaNum           -- "\w"
    | Disj [[Pattern]]     -- "[...]" or "(...|...)"
    | Char Char          -- "a" 
    | Neg [Char]         -- "[^...]"
    | Start              -- "^"
    | End                -- "$"
    | OneOrMore Pattern  -- "+"
    | ZeroOrMore Pattern -- "*"
    | Optional Pattern   -- "?"
    | Group [Pattern]    -- "(...)"
    | Wildcard           -- "."
    | BackRef Int        -- "\1"
    deriving Show

type Parser = Parsec Void T.Text

type PatternParser = Parser Pattern
type FullPatternParser = Parser [Pattern]
type Matcher = FullPattern -> Parser ()

type FullPattern = [Pattern]

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

backReference :: PatternParser
backReference = BackRef <$> (char '\\' *> (read <$> many (satisfy isDigit)))

optionalParser :: PatternParser
optionalParser = patternParser >>= (\patt -> char '?' $> Optional patt) <?> "zeroOrOne"

oneOrMoreParser :: PatternParser
oneOrMoreParser = patternParser >>= (\patt -> char '+' $> OneOrMore patt) <?> "oneOrMore"

zeroOrMoreParser :: PatternParser
zeroOrMoreParser = patternParser >>= (\patt -> char '*' $> ZeroOrMore patt) <?> "zeroOrMore"

completePatterParser :: FullPatternParser
completePatterParser = (optional startParser >>= maybe (pure id) (const $ pure (Start :))) <*> ((many patternWithRepetitionParser <**> (optional endParser >>= maybe (pure id) (const $ pure (++ [End])))) <* eof)

patternParser :: PatternParser
patternParser = choice . fmap try $ [digitParser, alphaNumParser, negativeParser, disjointCharParser, disjointPatternParser, groupParser, wildcardParser, charParser, backReference]

patternWithRepetitionParser :: PatternParser
patternWithRepetitionParser = choice . fmap try $ [ oneOrMoreParser, zeroOrMoreParser, optionalParser, patternParser]

groupParser :: PatternParser
groupParser = (Group <$> (char '(' *> manyTill patternWithRepetitionParser (char ')'))) <?> "grouped"

disjointCharParser :: PatternParser
disjointCharParser = (Disj . L.singleton <$> (char '[' *> manyTill charParser (char ']'))) <?> "choice"

disjointPatternParser :: PatternParser
disjointPatternParser = (Disj <$> (char '(' *> ((:) <$> many patternWithRepetitionParser <*> manyTill (char '|' *> many patternWithRepetitionParser) (char ')')))) <?> "disjoint"

negativeParser :: PatternParser
negativeParser = (Neg <$> (string "[^" *> manyTill (charParser >>= returnChar) (char ']'))) <?> "negativeGroup"
    where
        returnChar t = case t of
            Char c -> pure c
            _      -> error "Char pattern expected"

match:: Matcher
match = matchWithGroups []
    where
        matchWithGroups _ [] = pure () <?> "seqEmpty"
        matchWithGroups gs (Digit:ps) = satisfy isDigit *> matchWithGroups gs ps
        matchWithGroups gs (AlphaNum:ps) = satisfy isAlphaNum *> matchWithGroups gs ps
        matchWithGroups gs (Char c: ps) = satisfy (==c) *> matchWithGroups gs ps
        matchWithGroups _ (Disj []:_) = error "this cannot be" <?> "disjEmpty"
        matchWithGroups gs (Disj (p:ps):xs) = try (matchWithGroups gs (p ++ xs)) <|> matchWithGroups gs (Disj ps:xs) <?> "disj"
        matchWithGroups gs (Neg []:xs) = (anySingle $> ()) <?> "negGroupEmpty"
        matchWithGroups gs (Neg (p:ps):xs) = try (char p *> error "This is an error") <|> matchWithGroups gs (Neg ps:xs) <?> "negGroup"
        matchWithGroups gs (((Optional p):ps)) = try (matchWithGroups gs (p:ps)) <|> matchWithGroups gs ps <?> "seqZeroOrMore"
        matchWithGroups gs (((ZeroOrMore p):ps)) = try (matchWithGroups gs (p:ZeroOrMore p:ps)) <|> matchWithGroups gs ps <?> "seqZeroOrMore"
        matchWithGroups gs (((OneOrMore p):ps)) = matchWithGroups gs (p:ZeroOrMore p:ps) <?> "seqZeroOrMore"
        matchWithGroups _ (Start:ps) = do
            state <- getParserState
            let processed = stateOffset state
            if processed == 0
                then pure ()
                else error "No match"
        matchWithGroups _ [End] = eof
        matchWithGroups _ (End:xs) = error "Expected end of input"
        matchWithGroups gs (Wildcard:xs) = (anySingle *> matchWithGroups gs xs) <?> "wildcard"
        matchWithGroups gs (Group ps: xs) = do
            statusA <- getParserState
            let input = stateInput statusA
            let initial = stateOffset statusA
            _ <- matchWithGroups gs ps
            statusB <- getParserState
            let end = stateOffset statusB
            let consumed = T.take (end-initial) input
            let patt = Char <$> T.unpack consumed
            matchWithGroups (gs ++ [patt]) xs
        matchWithGroups gs (BackRef x: ps) = matchWithGroups gs ((gs!!x) ++ ps)


partialMatch :: Matcher -> Matcher
partialMatch match p = skipManyTill anySingle (try $ match p) <* many anySingle <* eof

isAlphaNum :: Char -> Bool
isAlphaNum c = (c <= 'z') && (c >= 'a') || isDigit c || (c <= 'Z') && (c >= 'A') || c == '_'
