module Spec.Aoc2021.Day3Spec where

import Aoc2021.Day3
    ( charBin2dec, parseLines, day3Part1, day3Part2FromInput )
import Spec.Aoc2021.Common
  ( Day (Day3),
    PuzzleInput (Puzzle, PuzzleExample1),
    readPuzzleInput,
  )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit (testCase, (@?=))

test_tests :: TestTree
test_tests =
  let day = Day3
   in testGroup
        "Unit tests Day3"
        [ testCase "test parse lines bin to dec" $
            charBin2dec "1001" @?= 9,
          testCase "part1 example file " $ do
            content <- parseLines <$> readPuzzleInput day PuzzleExample1
            let v = day3Part1 <$> content
            v @?= Right 198,
          testCase "part1 data file" $ do
            content <- parseLines <$> readPuzzleInput day Puzzle
            let v = day3Part1 <$> content
            v @?= Right 3277364,
          testCase "part2 example file " $ do
            content <- readPuzzleInput day PuzzleExample1
            let v = day3Part2FromInput content
            v @?= Right 230,
          testCase "part2 data file" $ do
            content <- readPuzzleInput day Puzzle
            let v = day3Part2FromInput content
            v @?= Right 5736383
        ]