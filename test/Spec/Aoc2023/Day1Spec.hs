module Spec.Aoc2023.Day1Spec where

import Aoc2023.Day1
  ( numberWordParser,
    parseLine,
    problem1,
    problem2,
    singleDigitOrnumberWordParser,
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
      year = Aoc2023
   in testGroup
        ("Unit tests parsing year" ++ show year ++ " day" ++ show day)
        [ testCase "test word parser" $
            parse numberWordParser "" "one" @?= Right 1
        , testCase "numberParser  test2" $
            parse parseDigitAndRest "" "twone" @?= Right (2, "wone")
        , testCase "numberParser  test2" $
            parse (parseLine singleDigitOrnumberWordParser) "" "twone" @?= Right [2, 1]
        , testCase "part1 example file " $ do
            v <- problem1 <$> readPuzzleInput year day PuzzleExample1
            v @?= Right 142
        , testCase "part1 real file " $ do
            v <- problem1 <$> readPuzzleInput year day Puzzle
            v @?= Right 54916
        , testCase "part2 example file " $ do
            v <- problem2 <$> readPuzzleInput year day Puzzle2Example1
            v @?= Right 281
        , testCase "part2 real file " $ do
            v <- problem2 <$> readPuzzleInput year day Puzzle
            v @?= Right 54728
        , testCase "numberParser example 1 prob1" asertInlineProb1example
        ] ::
        TestTree

parseDigitAndRest :: Parser (Int, String)
parseDigitAndRest = do
  digit <- numberWordParser
  rest <- many1 anyChar
  return (digit, rest)

asertInlineProb1example :: Assertion
asertInlineProb1example =
  let input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
      expected = 142
   in problem1 input @?= Right expected
   
   
