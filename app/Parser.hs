{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (partialMatch, match, PatternParser (..), Pattern (..), completePatterParser) where

import Control.Applicative ((<**>), (<|>))
import Control.Applicative.Combinators (option, optional, skipManyTill)
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.List as L (singleton)
import Data.Set qualified as Set (empty)
import Data.Text as T (Text, take, unpack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, anySingle, choice, getParserState, many, manyTill, noneOf, notFollowedBy, oneOf, satisfy, stateInput, stateOffset, token, try, (<?>), parseError, ParseError(..))
import qualified Text.Megaparsec as MP (match)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Debug (dbg')

data Pattern
  = Digit -- "\d"
  | AlphaNum -- "\w"
  | Alt [Pattern] [Pattern] -- "(...|...)"
  | Pos [Char]
  | Char Char -- "a"
  | Neg [Char] -- "[^...]"
  | Start -- "^"
  | End -- "$"
  | OneOrMore Pattern -- "+"
  | ZeroOrMore Pattern -- "*"
  | Optional Pattern -- "?"
  | Group [Pattern] -- "(...)"
  | Wildcard -- "."
  | BackRef Int -- "\1"
  deriving (Show)

type Parser = Parsec Void T.Text

type PatternParser = Parser Pattern

type FullPatternParser = Parser [Pattern]

type Matcher = FullPattern -> Parser ()

type FullPattern = [Pattern]

reservedChars :: String
reservedChars = "[]^\\$+*()?.|/"

digitParser :: PatternParser
digitParser = string "\\d" $> Digit

alphaNumParser :: PatternParser
alphaNumParser = string "\\w" $> AlphaNum

scapedCharParser :: PatternParser
scapedCharParser = Char <$> (char '\\' *> oneOf reservedChars)

regularCharParser :: PatternParser
regularCharParser = Char <$> noneOf reservedChars

charParser :: PatternParser
charParser = try scapedCharParser <|> regularCharParser

startParser :: PatternParser
startParser = char '^' $> Start

endParser :: PatternParser
endParser = char '$' $> End

wildcardParser :: PatternParser
wildcardParser = char '.' $> Wildcard

backReference :: PatternParser
backReference = BackRef <$> (char '\\' *> (read <$> many (satisfy isDigit)))

optionalParser :: PatternParser
optionalParser = patternParser >>= (\patt -> char '?' $> Optional patt)

oneOrMoreParser :: PatternParser
oneOrMoreParser = patternParser >>= (\patt -> char '+' $> OneOrMore patt)

zeroOrMoreParser :: PatternParser
zeroOrMoreParser = patternParser >>= (\patt -> char '*' $> ZeroOrMore patt)

completePatterParser :: FullPatternParser
completePatterParser =
  (optional startParser >>= maybe (pure id) (const $ pure (Start :)))
    <*> ((many patternWithRepetitionParser <**> (optional endParser >>= maybe (pure id) (const $ pure (++ [End])))) <* eof)

patternParser :: PatternParser
patternParser =
  choice . fmap try $
    [ digitParser,
      alphaNumParser,
      negativeParser,
      groupParser,
      disjointCharParser,
      disjointPatternParser,
      groupParser,
      wildcardParser,
      charParser,
      backReference
    ]

patternWithRepetitionParser :: PatternParser
patternWithRepetitionParser = choice . fmap try $ [oneOrMoreParser, zeroOrMoreParser, optionalParser, patternParser]

groupParser :: PatternParser
groupParser = Group <$> (char '(' *> manyTill patternWithRepetitionParser (char ')'))

disjointCharParser :: PatternParser
disjointCharParser = Pos <$> (char '[' *> manyTill ((\(Char c) -> c) <$> charParser) (char ']'))

disjointPatternParser :: PatternParser
disjointPatternParser = char '(' *> (Alt <$> many patternWithRepetitionParser) <*> (char '|' *> many patternWithRepetitionParser) <* char ')'

negativeParser :: PatternParser
negativeParser = Neg <$> (string "[^" *> manyTill (charParser >>= returnChar) (char ']'))
  where
    returnChar t = case t of
      Char c -> pure c
      _ -> error "Char pattern expected"

match :: Matcher
match = matchWithGroups []
  where
    matchWithGroups _  [] = pure ()
    matchWithGroups gs (Digit : ps) =  satisfy isDigit *> matchWithGroups gs ps
    matchWithGroups gs (AlphaNum : ps) = satisfy isAlphaNum *> matchWithGroups gs ps
    matchWithGroups gs (Char c : ps) = char c *> matchWithGroups gs ps
    matchWithGroups _  (Pos [] : _) = do
        state <- getParserState
        let processed = stateOffset state
        parseError $ FancyError processed Set.empty
    matchWithGroups gs (Alt p1 p2 : xs) = try (matchWithGroups gs (p1 ++ xs)) <|> matchWithGroups gs (p2 ++ xs)
    matchWithGroups gs (Pos (c : cs) : xs) = matchWithGroups gs (Alt [Char c] [Pos cs] : xs)
    matchWithGroups gs (Neg [] : xs) = anySingle $> ()
    matchWithGroups gs (Neg (p : ps) : xs) = notFollowedBy (char p) *> matchWithGroups gs (Neg ps : xs)
    matchWithGroups gs (((Optional p) : ps)) = try (matchWithGroups gs (p : ps)) <|> matchWithGroups gs ps
    matchWithGroups gs (((ZeroOrMore p) : ps)) = try (matchWithGroups gs (p: ZeroOrMore p : ps)) <|> matchWithGroups gs ps
    matchWithGroups gs (((OneOrMore p) : ps)) = matchWithGroups gs (p : ZeroOrMore p : ps)
    matchWithGroups gs (Start : ps) = do
      state <- getParserState
      let processed = stateOffset state
      if processed == 0
        then matchWithGroups gs ps
        else parseError $ FancyError processed Set.empty
    matchWithGroups _ [End] = eof
    matchWithGroups _ (End : xs) = error "Invalid Pattern"
    matchWithGroups gs (Wildcard : xs) = anySingle *> matchWithGroups gs xs
    matchWithGroups gs (Group ps : xs) = do
      (consumed, _) <-  dbg' "groupDbg" $ MP.match (matchWithGroups gs ps)
      let patt = Char <$> T.unpack consumed
      matchWithGroups (gs ++ [patt]) xs
    matchWithGroups gs (BackRef x : ps) = matchWithGroups gs ((gs !! (x - 1)) ++ ps)

partialMatch :: Matcher -> Matcher
partialMatch match p = skipManyTill anySingle (try $ match p) <* many anySingle <* eof

isAlphaNum :: Char -> Bool
isAlphaNum c = (c <= 'z') && (c >= 'a') || isDigit c || (c <= 'Z') && (c >= 'A') || c == '_'
