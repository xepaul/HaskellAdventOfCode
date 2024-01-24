{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Aoc2023.Day5
   where

import Data.Char (isLetter)
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.Parsec
  ( Parser,
  )
import Control.Monad.Combinators (sepBy1)
import Control.Monad.Combinators (optional)

import Text.Parsec hiding (optional, sepBy1)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')

data MapRow = MapRow { resetValue :: Int, startValue :: Int, rangeValue::Int} deriving(Show,Eq)

data MapValue = MapValue String String [MapRow] deriving (Show,Eq)

data SeedDataset = SeedDataset { seeds :: [Int], mapRows :: [MapValue]} deriving (Show,Eq)
data SeedPairsDataset = SeedPairsDataset { seedPairs :: [(Int,Int)], pairMapRows :: [MapValue]} deriving (Show,Eq)

parseSeeds :: Parser [Int]
parseSeeds = do
  _ <- string "seeds:"
  _ <- char ' '
  sepBy1 (read <$> many1 digit) (char ' ')

mapValue2 = do
  sourceName <- manyTill (satisfy isLetter) (try $ string "-to-")
  destName <- manyTill (satisfy isLetter) (try $ string " map:\n")
  rows <- many1 (mapRow <* optional endOfLine)
  return $ MapValue sourceName destName rows
  where
    number :: Parser Int
    number = read <$> many1 digit
    mapRow :: Parser MapRow
    mapRow = MapRow <$> (number <* char ' ') <*> (number <* char ' ') <*> number

parseSeedDataset :: Parser SeedDataset
parseSeedDataset = do
  seeds <- parseSeeds
  _ <- endOfLine
  _ <- endOfLine
  mapRows <- sepBy1  mapValue2 endOfLine
  return $ SeedDataset seeds mapRows


parseSeedPaairs :: Parser [(Int,Int)]
parseSeedPaairs = do
  _ <- string "seeds:"
  _ <- char ' '
  sepBy1 parseSeedPaair (char ' ')
  where
    numberparse :: Parser Int
    numberparse = read <$> many1 digit
    parseSeedPaair :: Parser (Int, Int)
    parseSeedPaair = do
      a <- numberparse
      _ <- char ' '
      b <- numberparse
      return (a,b)

parseSeedPairsDataset :: Parser SeedPairsDataset
parseSeedPairsDataset = do
  seeds <- parseSeedPaairs
  _ <- endOfLine
  _ <- endOfLine
  mapRows <- sepBy1  mapValue2 endOfLine
  return $ SeedPairsDataset seeds mapRows


calc :: Int -> MapRow -> Maybe Int
calc v (MapRow reset start range)
  | v < start = Nothing
  | v > start + range = Nothing
  | otherwise = Just $ reset + (v - start)

calcReadValue :: Int -> [MapRow] -> Int
calcReadValue value mapRows =
  case mapMaybe (calc value) mapRows of
    [] -> value
    (x:_) -> x

solverCommon :: String -> Either ParseError Int
solverCommon = fmap calcWithData . parse parseSeedDataset ""
  where
    calcWithData :: SeedDataset -> Int
    calcWithData (SeedDataset {seeds = seedValues, mapRows = rows}) =
      let convMap = mapValuesToMap rows
      in minimum $ map (calcFlow convMap "seed" ) seedValues   
    calcFlow :: Map String MapValue -> String -> Int -> Int
    calcFlow mapValues key value
      | key == "location" = value
      | otherwise = case Map.lookup key mapValues of
          Nothing -> error $ "Key not found: " ++ key
          Just (MapValue _ destName rows) -> calcFlow mapValues destName (calcReadValue value rows)

solverCommon2 :: String -> Either ParseError Int
solverCommon2 = fmap calcWithData . parse parseSeedPairsDataset ""
  where
    calcWithData :: SeedPairsDataset -> Int
    calcWithData (SeedPairsDataset {seedPairs = seedValues, pairMapRows = rows}) =
      let convMap = mapValuesToMap rows
      in foldl' min maxBound $ map (calcFlow convMap "seed") $ concatMap (\(a,b) -> take b [a..]) seedValues
      where 
        calcFlow :: Map String MapValue -> String -> Int -> Int
        calcFlow mapValues= go
          where
            go key value
              | key == "location" = value
              | otherwise = case Map.lookup key mapValues of
                  Nothing -> error $ "Key not found: " ++ key
                  Just (MapValue _ destName rows) -> go destName (calcReadValue value rows)                    

mapValuesToMap :: [MapValue] -> Map String MapValue
mapValuesToMap mapValues = Map.fromList [(key, mv) | mv@(MapValue key _ _) <- mapValues]