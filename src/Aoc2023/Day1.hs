module Aoc2023.Day1 where

import Data.Char (isLetter)
import Data.Maybe (catMaybes)
import Text.Parsec.Char (digit, string)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (lookAhead, many, parse, try, (<|>))
import Text.ParserCombinators.Parsec
  ( Parser,
    anyChar,
    char,
    satisfy,
    sepEndBy,
  )

problem1 :: String -> Either ParseError Int
problem1 = solverCommon singleDigitParser

problem2 :: String -> Either ParseError Int
problem2 = solverCommon (singleDigitParser <|> numberWordParser)


solverCommon :: Parser Int -> String -> Either ParseError Int
solverCommon p = fmap (sum . fmap sumOfNumbers) . parse parseLines ""
  where
    sumOfNumbers vv = 10 * head vv + last vv
    parseLines = parseLine `sepEndBy` char '\n'

singleDigitParser :: Parser Int
singleDigitParser = (\x -> read [x]) <$> digit

numberWordParser :: Parser Int
numberWordParser = do
  v <- lookAhead digitTextParser
  _ <- anyChar
  return v
  where
    digitTextParser :: Parser Int
    digitTextParser =
      try (string "one" >> return 1)
        <|> try (string "two" >> return 2)
        <|> try (string "three" >> return 3)
        <|> try (string "four" >> return 4)
        <|> try (string "five" >> return 5)
        <|> try (string "six" >> return 6)
        <|> try (string "seven" >> return 7)
        <|> try (string "eight" >> return 8)
        <|> try (string "nine" >> return 9)

parseLine :: Parser (Maybe Int)
parseLine = catMaybes <$> many (Just <$> p <|> Nothing <$ satisfy isLetter)
    parseLine = catMaybes <$> many (Just <$> p <|> Nothing <$ satisfy isLetter)

singleDigitParser :: Parser Int
singleDigitParser = (\x -> read [x]) <$> digit

numberWordParser :: Parser Int
numberWordParser = do
  v <- lookAhead digitTextParser
  _ <- anyChar
  return v
  where
    digitTextParser :: Parser Int
    digitTextParser =
      try (string "one" >> return 1)
        <|> try (string "two" >> return 2)
        <|> try (string "three" >> return 3)
        <|> try (string "four" >> return 4)
        <|> try (string "five" >> return 5)
        <|> try (string "six" >> return 6)
        <|> try (string "seven" >> return 7)
        <|> try (string "eight" >> return 8)
        <|> try (string "nine" >> return 9)
