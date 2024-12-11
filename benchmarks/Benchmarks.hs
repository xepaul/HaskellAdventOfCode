module Main where

import Criterion.Main
import Aoc2024.Day1 (prop1,prop1a'''',prop1a''', parseTuples)
import Common (readPuzzleInput, AocYear(Aoc2024), PuzzleInput(..), Day(Day1))

main :: IO ()
main = do
  input <- readPuzzleInput Aoc2024 Day1 PuzzleExample1
  let parsed = case parseTuples input of
                 Right tuples -> tuples
                 Left _ -> []
  input1 <- readPuzzleInput Aoc2024 Day1 Puzzle
  let parsed1 = case parseTuples input1 of
                 Right tuples -> tuples
                 Left _ -> []
  defaultMain [
      bgroup "prob1 benchs example input1" [
          bench "prop1 example input" $ nf prop1 parsed,
          bench "prop1a'''  example input" $ nf prop1a'''  parsed,
          bench "prop1a'''' example input" $ nf prop1a'''' parsed
        ],
      bgroup "prob1 benchs  input1" [
          bench "prop1 input1" $ nf prop1 parsed1,
          bench "prop1a'''  input1" $ nf prop1a'''  parsed1,
          bench "prop1a'''' input1" $ nf prop1a'''' parsed1
        ]
    ]