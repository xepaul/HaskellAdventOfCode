{-# LANGUAGE BangPatterns #-}

module Aoc2021.Day1 where

import Data.Either.Extra (mapLeft)
import Data.Functor ((<&>))
import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
  ( char,
    sepEndBy1,
  )
import Text.ParserCombinators.Parsec.Number (int)

-- | Count the items that meet the predicate
--
-- Examples
-- >>> count even [0::Int,1,2,3,4,5]
-- 3
count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where
    go !n [] = n
    go !n (x : xs)
      | p x = go (n + 1) xs
      | otherwise = go n xs

-- | create a list of overlapping pairs
--
-- Examples
-- >>> pairwise [0::Int,1,2,3]
-- [(0,1),(1,2),(2,3)]
pairwise :: [b] -> [(b, b)]
pairwise [] = []
pairwise y = zip y (tail y)

-- | create a list of overlapping triplets
--
-- Examples
-- >>> triplewise [0::Int,1,2,3,4]
-- [(0,1,2),(1,2,3),(2,3,4)]
triplewise :: [c] -> [(c, c, c)]
triplewise [] = []
triplewise [_] = []
triplewise [_, _] = []
triplewise y = zip3 y (tail y) (tail (tail y))

parseLines :: String -> Either String [Int]
parseLines = mapLeft show . parse inputParser ""
  where
    inputParser = sepEndBy1 int (char '\n')

prob1 :: [Int] -> Int
prob1 = count (\(a, b) -> b > a) . pairwise

prob2 :: [Int] -> Int
prob2 =
  count (\(a, b) -> b > a)
    . pairwise
    . map (\(a, b, c) -> a + b + c)
    . triplewise

day1Part1FromInput :: String -> Either String Int
day1Part1FromInput l = parseLines l <&> prob1

day1Part2FromInput :: String -> Either String Int
day1Part2FromInput l = parseLines l <&> prob2

exampleInput :: [Int]
exampleInput = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

-- >>> prob1 exampleInput
-- 7

-- >>> prob2 exampleInput
-- 5
