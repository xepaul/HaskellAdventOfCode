module Spec.Aoc2023.Day1_2Spec where

import Aoc2023.Day1_2 (prob1)
import Spec.Aoc2023.Common (Day (Day1))
import Spec.Common (AocYear (Aoc2023), PuzzleInput (Puzzle, PuzzleExample1,Puzzle2Example1), readPuzzleInput)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Parsec.Prim (parse, (<|>))

test_tests :: TestTree
test_tests =
  let day = Day1
      year = Aoc2023
   in testGroup
        ("Unit tests simple year " ++ show year ++ " day" ++ show day)
        [ testcase1,
          testCase "part1 example file " $ do
            v <- prob1 <$> readPuzzleInput year day PuzzleExample1
            v @?= 142,
          testCase "part1 real file " $ do
            v <- prob1 <$> readPuzzleInput year day Puzzle
            v @?= 54916
        ]

testcase1 =
  let input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
      expected = 142
   in testCase "numberParser example 1 prob1" $
        prob1 input @?= expected
