module Spec.Aoc2024.Day4Spec where

import Aoc2024.Day4
  
import Spec.Aoc2024.Common
  ( Day (Day4),
  )
import Spec.Common
  ( AocYear (Aoc2024),
    PuzzleInput (Puzzle, PuzzleExample1),
    readPuzzleInput,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

test_tests :: TestTree
test_tests =
  let day = Day4
      year = Aoc2024
   in testGroup
        ("Unit tests parsing year" ++ show year ++ " day" ++ show day)
        [ 

        testCase "part1 example file " $ do
            input <- readPuzzleInput year day PuzzleExample1           
            let result = prob1 input
            result @?=  18
        ,testCase "part1  file " $ do
            input <- readPuzzleInput year day Puzzle           
            let result = prob1 input
            result @?=  2401
        ,testCase "part2 example file " $ do
            input <- readPuzzleInput year day PuzzleExample1           
            let result = prob2 input
            result @?=  9
          ,testCase "part2  file " $ do
            input <- readPuzzleInput year day Puzzle           
            let result = prob2 input
            result @?=  1822
        ] ::
        TestTree



