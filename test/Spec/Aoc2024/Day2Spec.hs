module Spec.Aoc2024.Day2Spec where

import Aoc2024.Day2 ( simpleReportsParser, prob1 ,prob2)
  
import Spec.Aoc2024.Common
  ( Day (Day2),
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
  let day = Day2
      year = Aoc2024
   in testGroup
        ("Unit tests parsing year" ++ show year ++ " day" ++ show day)
        [ 
         testCase "test word parser" $
            simpleReportsParser "3 4\n7 9\n8 9" @?= Right [[3,4],[7,9],[8,9]]
        , testCase "rec2 safe progression" $
            prob1 [[1, 2, 3, 4]] @?= 1
        , testCase "rec2 unsafe progression" $
            prob1 [[1, 2, 4, 4]] @?= 0      
        , testCase "rec2 out of range" $
            prob1  [[1,5, 6]] @?= 0
        ,testCase "part1 example file " $ do
            input <- readPuzzleInput year day PuzzleExample1           
            let result = prob1 <$> simpleReportsParser input
            result @?= Right 2
        , testCase "part1  file line count check " $ do
            input <- readPuzzleInput year day Puzzle           
            let result = (length) <$> simpleReportsParser input
            result @?= Right 1000
        , testCase "part1  file " $ do
            input <- readPuzzleInput year day Puzzle           
            let result = prob1 <$> simpleReportsParser input
            result @?= Right 299

        ,testCase "part2 example file " $ do
            input <- readPuzzleInput year day PuzzleExample1           
            let result = prob2 <$> simpleReportsParser input
            result @?= Right 4
        , testCase "part2  file " $ do
            input <- readPuzzleInput year day Puzzle           
            let result = prob2 <$> simpleReportsParser input
            result @?= Right 364

        ] ::
        TestTree



