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

data DigitText = OneText | TwoText | ThreeText | FourText | FiveText | SixText | SevenText | EightText | NineText deriving (Show)

oneTextParser :: Parser DigitText
oneTextParser = do
  string "one"
  return OneText

twoTextParser :: Parser DigitText
twoTextParser = do
  string "two"
  return TwoText

threeTextParser :: Parser DigitText
threeTextParser = do
  string "three"
  return ThreeText

fourTextParser :: Parser DigitText
fourTextParser = do
  string "four"
  return FourText

fiveTextParser :: Parser DigitText
fiveTextParser = do
  string "five"
  return FiveText

sixTextParser :: Parser DigitText
sixTextParser = do
  string "six"
  return SixText

sevenTextParser :: Parser DigitText
sevenTextParser = do
  string "seven"
  return SevenText

eightTextParser :: Parser DigitText
eightTextParser = do
  string "eight"
  return EightText

nineTextParser :: Parser DigitText
nineTextParser = do
  string "nine"
  return NineText

convertDigitTextToChar :: DigitText -> Char
convertDigitTextToChar dt = case dt of
  OneText -> '1'
  TwoText -> '2'
  ThreeText -> '3'
  FourText -> '4'
  FiveText -> '5'
  SixText -> '6'
  SevenText -> '7'
  EightText -> '8'
  NineText -> '9'

digitTextParser :: Parser DigitText
digitTextParser =
  try oneTextParser
    <|> try twoTextParser
    <|> try threeTextParser
    <|> try fourTextParser
    <|> try fiveTextParser
    <|> try sixTextParser
    <|> try sevenTextParser
    <|> try eightTextParser
    <|> try nineTextParser

numberWordParser :: Parser Char
numberWordParser = do
  v <- lookAhead (convertDigitTextToChar <$> digitTextParser)
  _ <- anyChar
  return v

numberParser :: Parser (Maybe Int)
numberParser = (Just . (\x -> read [x]) <$> digit) <|> Nothing <$ satisfy isLetter

numberParserAndWords :: Parser (Maybe Int)
numberParserAndWords = ((\x -> Just $ read [x]) <$> (digit <|> numberWordParser)) 
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
