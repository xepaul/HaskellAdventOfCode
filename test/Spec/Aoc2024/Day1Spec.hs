module Spec.Aoc2024.Day1Spec where

import Aoc2024.Day1
  
import Spec.Aoc2024.Common
  ( Day (Day1),
  )
import Spec.Common
  ( AocYear (Aoc2024),
    PuzzleInput (Puzzle, Puzzle2Example1, PuzzleExample1),
    readPuzzleInput,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
  ( Parser,
    anyChar,
  )

test_tests :: TestTree
test_tests =
  let day = Day1
      year = Aoc2024
   in testGroup
        ("Unit tests parsing year" ++ show year ++ " day" ++ show day)
        [ testCase "test word parser" $
            parseTuples  exampleInput @?= Right [(3,4),(4,3),(2,5),(1,3),(3,9),(3,3)]
        , testCase "part1 example file " $ do
            input <- readPuzzleInput year day PuzzleExample1           
            let result = prop1 <$> parseTuples input
            result @?= Right 11
        , testCase "part1  file " $ do
            input <- readPuzzleInput year day Puzzle           
            let result = prop1 <$> parseTuples input
            result @?= Right 1765812
        , testCase "part2 example file " $ do
            input <- readPuzzleInput year day PuzzleExample1           
            let result = prob2 <$> parseTuples input
            result @?= Right 31
        , testCase "part2  file " $ do
            input <- readPuzzleInput year day Puzzle           
            let result = prob2 <$> parseTuples input
            result @?= Right 1765812
        ] ::
        TestTree



