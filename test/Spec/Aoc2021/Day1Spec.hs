module Spec.Aoc2021.Day1Spec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Aoc2021.Day1
import Spec.Aoc2021.Common
  ( Day (Day1),
  )
import Spec.Common
  ( AocYear (Aoc2021),
    PuzzleInput (Puzzle, PuzzleExample1),
    readPuzzleInput,
  )

test_tests :: TestTree
test_tests =
  let day = Day1
   in testGroup
        "Unit tests day1"
        [ testCase "prob1" $
            prob1 [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] @?= 7,
          testCase "part1 example file " $ do
            content <- parseLines <$> readPuzzleInput Aoc2021 day PuzzleExample1
            let v = prob1 <$> content
            v @?= Right 7,
          testCase "part1  file " $ do
            content <- parseLines <$> readPuzzleInput Aoc2021 day Puzzle
            let v = prob1 <$> content
            v @?= Right 1342,
          testCase "part1 example file " $ do
            v <- day1Part1FromInput <$> readPuzzleInput Aoc2021 day PuzzleExample1
            v @?= Right 7,
          testCase "part2 example file2 " $ do
            content <- parseLines <$> readPuzzleInput Aoc2021 day PuzzleExample1
            let v = prob2 <$> content
            v @?= Right 5,
          testCase "part2 file " $ do
            v <- day1Part2FromInput <$> readPuzzleInput Aoc2021 day Puzzle
            v @?= Right 1378
        ]