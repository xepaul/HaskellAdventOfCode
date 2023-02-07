{-# LANGUAGE ImportQualifiedPost #-}
module Spec.Aoc2021.Day3Spec where

import Aoc2021.Day3
  ( charBin2dec,
    day3Part1,
    day3Part2FromInput,
    parseLines, dec2bin,
  )
import Spec.Aoc2021.Common
  ( Day (Day3),
  )
import Spec.Common
  ( AocYear (Aoc2021),
    PuzzleInput (Puzzle, PuzzleExample1),
    readPuzzleInput,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty.Hedgehog qualified as H
import Hedgehog
test_tests :: TestTree
test_tests =
  let day = Day3
   in testGroup
        "Unit tests Day3"
        [ 
          testCase "test parse lines bin to dec" $
            charBin2dec "1001" @?= 9,
          testCase "part1 example file " $ do
            content <- parseLines <$> readPuzzleInput Aoc2021 day PuzzleExample1
            let v = day3Part1 <$> content
            v @?= Right 198,
          testCase "part1 data file" $ do
            content <- parseLines <$> readPuzzleInput Aoc2021 day Puzzle
            let v = day3Part1 <$> content
            v @?= Right 3277364,
          testCase "part2 example file " $ do
            content <- readPuzzleInput Aoc2021 day PuzzleExample1
            let v = day3Part2FromInput content
            v @?= Right 230,
          testCase "part2 data file" $ do
            content <- readPuzzleInput Aoc2021 day Puzzle
            let v = day3Part2FromInput content
            v @?= Right 5736383,

          H.testProperty "test Fast tripping" propbin2dec
        ]


numbers :: Gen Int
numbers = Gen.int (Range.linear 0 maxBound)

propbin2dec :: Property
propbin2dec =  Hedgehog.property $   do
  s <- forAll numbers
  Hedgehog.tripping s dec2bin cc2
  where 
        
      cc2 :: String -> Either String Int
      cc2 x = do Right $ charBin2dec x
      