{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ParallelListComp #-}

module Aoc2021.Day5 where

import Data.Either.Extra (mapLeft)
import Data.Map qualified as Map
import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    sepEndBy1,
    string,
  )
import Text.ParserCombinators.Parsec.Number (nat)

newtype Point = Point (Int, Int) deriving (Eq, Ord,Show)

data Line = Line Point Point deriving (Show,Eq)

data DiagonalOption = DontSupportDiagonals | SupportDiagonals deriving (Eq)

parseLines :: String -> Either String [Line]
parseLines = mapLeft show . parse inputParser ""
  where
    inputParser = sepEndBy1 parseLine (char '\n')
    parseLine = Line <$> parsePoint <*> (string " -> " *> parsePoint)
    parsePoint :: Parser Point
    parsePoint = curry Point <$> nat <*> (char ',' *> nat)

processLines :: DiagonalOption -> [Line] -> Int
processLines opt =
  length
    . filter (>= 2)
    . map snd
    . plotPoints
    . concatMap (rasterLine opt)
  where
    plotPoints :: [Point] -> [(Point, Int)]
    plotPoints = Map.toList . foldl (\s p -> Map.insertWith (+) p 1 s) Map.empty
    
rasterLine :: DiagonalOption -> Line -> [Point]
rasterLine diagonalOption (Line (Point (x1, y1)) (Point (x2, y2)))
  | x1 == x2 = [Point (x1, y1 + y * ydelta) | y <- [0 .. yabs]]
  | y1 == y2 = [Point (x1 + x * xdelta, y1) | x <- [0 .. xabs]]
  | xabs == yabs =
    if diagonalOption == DontSupportDiagonals
      then []
      else [Point (x1 + x * xdelta, y1 + y * ydelta) | x <- [0 .. xabs] | y <- [0 .. yabs]]
  | otherwise = []
  where
    xdelta = signum (x2 - x1)
    ydelta = signum (y2 - y1)
    xabs = abs (x2 - x1)
    yabs = abs (y2 - y1)

processInputPart1 :: String -> Either String Int
processInputPart1 s =
  processLines DontSupportDiagonals <$> parseLines s

processInputPart2 :: String -> Either String Int
processInputPart2 s =
  processLines SupportDiagonals <$> parseLines s