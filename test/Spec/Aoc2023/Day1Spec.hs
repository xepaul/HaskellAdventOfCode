module Spec.Aoc2023.Day1Spec where

import Aoc2023.Day1
  ( Info (..),
    numberParser,
    problem1,
    problem2,
  )
import Spec.Aoc2023.Common
  ( Day (Day1),
  )
import Spec.Common
  ( AocYear (Aoc2023),
    PuzzleInput (Puzzle, Puzzle2Example1, PuzzleExample1),
    readPuzzleInput,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Parsec.Prim (parse, (<|>))

test_tests :: TestTree
test_tests =
  let day = Day1
      year = Aoc2023
   in testGroup
        ("Unit tests parsing year" ++ show year ++ " day" ++ show day)
        [ --testCase_numberParser22,
          testCase "part1 example file " $ do
            v <- problem1 <$> readPuzzleInput year day PuzzleExample1
            v @?= Right 142,
          testCase "part1 real file " $ do
            v <- problem1 <$> readPuzzleInput year day Puzzle
            v @?= Right 54916,
          testCase "part2 example file " $ do
            v <- problem2 <$> readPuzzleInput year day Puzzle2Example1
            v @?= Right 281,      
          testCase "part2 real file " $ do
            v <- problem2 <$> readPuzzleInput year day Puzzle
            v @?= Right 54728
        ] ::
        TestTree

testCase_numberParser22 =
  let input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
      expected = 142
   in testCase "numberParser example 1 prob1" $
        problem1 input @?= Right expected
-- not 54702 
-- could be 54728