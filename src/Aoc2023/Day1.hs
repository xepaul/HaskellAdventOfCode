{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Aoc2023.Day1 where

import Data.Char (isControl, isDigit, isLetter)
import Data.Maybe (catMaybes, mapMaybe)
import Text.Parsec (getPosition, sourceColumn)
import Text.Parsec.Char (digit, letter, string)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (sourceLine)
import Text.Parsec.Prim (lookAhead, many, parse, try, (<|>))
import Text.ParserCombinators.Parsec
  ( Parser,
    anyChar,
    char,
    satisfy,
    sepEndBy,
  )
import Text.Read.Lex (isSymbolChar)

numberWordParser :: Parser ( Int)
numberWordParser = do
  v <- lookAhead (digitTextParser)
  _ <- anyChar
  return $  v
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



numberParser :: Parser (Maybe Int)
numberParser = (Just . (\x -> read [x]) <$> digit) <|> Nothing <$ satisfy isLetter

numberParserAndWords :: Parser (Maybe Int)
numberParserAndWords =  Just  <$> ((\x ->read [x] )<$>digit <|> numberWordParser)
                          <|> Nothing <$ satisfy isLetter

parseLines :: Parser [[Int]]
parseLines = sepEndBy parseString (char '\n')
  where
    parseString = catMaybes <$> many numberParser

parseLinesWithWords :: Parser [[Int]]
parseLinesWithWords = sepEndBy parseString (char '\n')
  where
    parseString = catMaybes <$> many numberParserAndWords

problem1 :: String -> Either ParseError Int
problem1 = fmap (sum . fmap sumOfNumbers) . parse parseLines ""
  where
    sumOfNumbers vv = 10 * head vv + last vv

problem2 :: String -> Either ParseError Int
problem2 = fmap (sum . fmap sumOfNumbers) . parse parseLinesWithWords ""
  where
    sumOfNumbers vv = 10 * head vv + last vv

exampleInput = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

exampleInput2 = "seven" -- 1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

-- >>> problem1a exampleInput
-- Right 142

-- Right 142
-- Prelude.read: no parse

-- >>> problem2 "two17"
-- Right 27

-- >>> problem2 "twoddnine"
-- >>> problem2 "xtwone"
-- Right 29
-- Right 21

-- >>> problem1a exampleInput2
-- Prelude.read: no parse
