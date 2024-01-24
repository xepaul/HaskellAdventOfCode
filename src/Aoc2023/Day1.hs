module Aoc2023.Day1
  ( problem1,
    problem2,
    singleDigitOrnumberWordParser,
    overlappingNumberWordParser,
    parseLine
  ) where

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
import Text.Parsec.Combinator (choice)

problem1 :: String -> Either ParseError Int
problem1 = solverCommon singleDigitParser

problem2 :: String -> Either ParseError Int
problem2 = solverCommon singleDigitOrnumberWordParser


solverCommon :: Parser Int -> String -> Either ParseError Int
solverCommon p = fmap (sum . fmap sumOfNumbers) . parse parseLines ""
  where
    sumOfNumbers vv = 10 * head vv + last vv
    parseLines = parseLine p `sepEndBy` char '\n'    

parseLine :: Parser Int -> Parser [Int]
parseLine p = catMaybes <$> many (Just <$> p <|> Nothing <$ satisfy isLetter)

singleDigitParser :: Parser Int
singleDigitParser = (\x -> read [x]) <$> digit

singleDigitOrnumberWordParser :: Parser Int
singleDigitOrnumberWordParser = singleDigitParser <|> overlappingNumberWordParser

overlappingNumberWordParser :: Parser Int
overlappingNumberWordParser = lookAhead digitTextParser <* anyChar
  where
    digitTextParser = choice $ map parseWord
      [ ("one", 1)
      , ("two", 2)
      , ("three", 3)
      , ("four", 4)
      , ("five", 5)
      , ("six", 6)
      , ("seven", 7)
      , ("eight", 8)
      , ("nine", 9)
      ]
    parseWord :: (String, Int) -> Parser Int
    parseWord (word, value) = try $ string word >> return value