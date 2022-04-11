module Spec.Aoc2021.Day1Spec where

import Aoc2021.Day1
import Spec.Aoc2021.Common
  ( Day (Day1),
    PuzzleInput (Puzzle, PuzzleExample1),
    readPuzzleInput,
  )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit (testCase, (@?=))

test_tests :: TestTree
test_tests =
  let day = Day1
   in testGroup
        "Unit tests day1"
        [ testCase "prob1" $
            prob1 [199,200,208,210,200,207,240,269,260,263] @?= 7
          , testCase "part1 example file " $ do
            content <- parseLines <$> readPuzzleInput day PuzzleExample1
            let v = prob1 <$> content
            v @?= Right 7
          , testCase "part1  file " $ do
            content <- parseLines <$> readPuzzleInput day Puzzle
            let v = prob1 <$> content
            v @?= Right 1342
          , testCase "part1 example file " $ do
            v <-  day1Part1FromInput <$> readPuzzleInput day PuzzleExample1           
            v @?= Right 7

          , testCase "part2 example file2 " $ do
            content <- parseLines <$> readPuzzleInput day PuzzleExample1
            let v = prob2 <$> content
            v @?= Right 5

          , testCase "part2 file " $ do
            v <- day1Part2FromInput <$> readPuzzleInput day Puzzle         
            v @?= Right 1378
        ]