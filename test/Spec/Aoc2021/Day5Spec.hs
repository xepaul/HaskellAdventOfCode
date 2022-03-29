module Spec.Aoc2021.Day5Spec where

import Aoc2021.Day5
import Spec.Aoc2021.Common
  ( Day (Day5),
    PuzzleInput (Puzzle, PuzzleExample1),
    readPuzzleInput,
  )
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

test_tests :: TestTree
test_tests =
  let day = Day5
   in testGroup
        "Unit tests Rosalind Revc"
        [ testCase "test parse lines" $
            parseLines "0,9 -> 5,9\n82,0 -> 0,8"
              @?= Right [Line (Point (0, 9)) (Point (5, 9)), Line (Point (82, 0)) (Point (0, 8))],
          testCase "test raster diagonal" $
            rasterLine SupportDiagonals (Line (Point (1, 1)) (Point (3, 3))) @?= [Point (1, 1), Point (2, 2), Point (3, 3)],
          testCase "test raster test skip diagonal" $
            rasterLine DontSupportDiagonals (Line (Point (1, 1)) (Point (3, 3))) @?= [],
          testCase "test raster line vert" $
            rasterLine DontSupportDiagonals . head <$> parseLines "1,1 -> 1,3" @?= Right [Point (1, 1), Point (1, 2), Point (1, 3)],
          testCase "test raster line hoz" $
            rasterLine DontSupportDiagonals . head <$> parseLines "9,7 -> 7,7" @?= Right [Point (9, 7), Point (8, 7), Point (7, 7)],
          testCase "test raster line dia1" $
            rasterLine SupportDiagonals . head <$> parseLines "1,1 -> 3,3" @?= Right [Point (1, 1), Point (2, 2), Point (3, 3)],
          testCase "test raster line dia2" $
            rasterLine SupportDiagonals . head <$> parseLines "9,7 -> 7,9" @?= Right [Point (9, 7), Point (8, 8), Point (7, 9)],
          testCase "test Example Data Part1" $ do
            v <- processInputPart1 <$> readPuzzleInput day PuzzleExample1
            v @?= Right 5,
          testCase "test Example Data Part2" $ do
            v <- processInputPart2 <$> readPuzzleInput day PuzzleExample1
            v @?= Right 12,
          testCase "test Data Part1" $ do
            v <- processInputPart1 <$> readPuzzleInput day Puzzle
            v @?= Right 5306,
          testCase "test Data Part2" $ do
            v <- processInputPart2 <$> readPuzzleInput day Puzzle
            v @?= Right 17787
        ]