
{-# LANGUAGE ImportQualifiedPost #-}
module Spec.Aoc2021.Day2Spec
    --(tests) 
    where

import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck.Arbitrary.ADT
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Spec.Common
  ( AocYear (Aoc2021),
    PuzzleInput (Puzzle, PuzzleExample1),
    readPuzzleInput,
  )
import Spec.Aoc2021.Common
    (  Day(Day2) )
import Aoc2021.Day2

test_tests :: TestTree
test_tests =
    let day = Day2 
    in
    testGroup "Unit tests Day2"
        [

         testCase "test forward parse" $
           parseInstructions "forward 7" @?= Right [Forward 7]

         , testCase "test forward parse state processing " $
           aimProcessInstructionsM3 <$> aimParseInstructions inlineExampleData @?= Right 900

          , testCase "test forward parse state processing " $
             aimProcessInstructionsM4 <$> aimParseInstructions inlineExampleData  @?= Right (Right 900)

        , testCase "test forward parse state processing " $
             aimProcessInstructionsM5 <$> aimParseInstructions inlineExampleData  @?= Right 900

         ,testCase "test up parse" $
           parseInstructions "up 7" @?= Right [Up 7]

         ,testCase "test Example Data Part1" $ do
            v <- processInputPart1 <$> readPuzzleInput Aoc2021 day PuzzleExample1
            v @?= Right 150

        , testCase "test Example Data Part2" $ do
            v <- processInputPart2 <$> readPuzzleInput Aoc2021 day PuzzleExample1
            v @?= Right 900

         , testCase "test Data Part1" $ do
            v <- processInputPart1 <$> readPuzzleInput Aoc2021 day Puzzle
            v @?= Right 1698735

        , testCase "test  Data Part2" $ do
            v <- processInputPart2 <$> readPuzzleInput Aoc2021 day Puzzle
            v @?= Right 1594785890
        ]


inlineExampleData = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"


instance Arbitrary HozPosition  where
  arbitrary = genericArbitrary
instance Arbitrary Instruction  where
  arbitrary = genericArbitrary

instance Arbitrary Depth  where
  arbitrary = genericArbitrary

instance ToADTArbitrary Depth
instance ToADTArbitrary HozPosition
instance ToADTArbitrary Instruction


xx () = generate (toADTArbitrary (Proxy :: Proxy Instruction ))
xxy () = generate (toADTArbitrary (Proxy :: Proxy Depth ))
