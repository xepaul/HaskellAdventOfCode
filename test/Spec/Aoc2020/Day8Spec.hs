module Spec.Aoc2020.Day8Spec where

import Aoc2020.Day8
    ( RegisterValue(RegisterValue),
      Address(Address),
      Instruction(Acc, Jmp),
      parseLines,
      day8Part1FromInput )
import Spec.Aoc2020.Common
  ( Day (Day8)
  )
import Spec.Common
  ( AocYear (Aoc2020),
    PuzzleInput (Puzzle, PuzzleExample1),
    readPuzzleInput,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

test_tests :: TestTree
test_tests =
  let day = Day8
   in testGroup
        "Unit tests Day8"
        [ testCase "part1 example file " $ do
            parseLines "jmp 100\nacc 300"
              @?= Right [Jmp (Address 100), Acc (RegisterValue 300)],
          testCase "part1 example file " $ do
            v <- day8Part1FromInput <$> readPuzzleInput Aoc2020 day PuzzleExample1
            v @?= Right 5,
          testCase "part1  file " $ do
            v <- day8Part1FromInput <$> readPuzzleInput Aoc2020 day Puzzle
            v @?= Right 1949
        ]