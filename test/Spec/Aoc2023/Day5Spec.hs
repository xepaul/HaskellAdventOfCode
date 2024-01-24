module Spec.Aoc2023.Day5Spec where

import Aoc2023.Day5 (parseSeedDataset)
import Spec.Aoc2023.Common (Day (Day5))
import Spec.Common (AocYear (Aoc2023), PuzzleInput (Puzzle, PuzzleExample1), readPuzzleInput)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Parsec.Prim (parse)
import Aoc2023.Day5 (SeedDataset(..))
import Aoc2023.Day5 (MapRow(..))
import Aoc2023.Day5 (MapValue(..))
import Aoc2023.Day5 (solverCommon, solverCommon2)

test_tests :: TestTree
test_tests =
  let day = Day5
      year = Aoc2023
  in testGroup
     ("Unit tests parsing year" ++ show year ++ " day" ++ show day)
     [ testCase "test word parser" $
       parse parseSeedDataset "" test1Data @?= Right test1Expected
     , testCase "part1 example file " $ do
       v <- solverCommon <$> readPuzzleInput year day PuzzleExample1
       v @?= Right 35
     , testCase "part1  file " $ do
       v <- solverCommon <$> readPuzzleInput year day Puzzle
       v @?= Right 26273516
     , testCase "part2 example file " $ do
       v <- solverCommon2 <$> readPuzzleInput year day PuzzleExample1
       v @?= Right 46
    --  , testCase "part2  file " $ do
    --    v <- solverCommon2 <$> readPuzzleInput year day Puzzle
    --    v @?= Right 34039469
     ] ::
     TestTree

test1Data =
  "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15"

test1Expected =
  SeedDataset
  { seeds = [79, 14, 55, 13]
  , mapRows =
    [ MapValue "seed" "soil"
      [ MapRow {resetValue = 50, startValue = 98, rangeValue = 2}
      , MapRow {resetValue = 52, startValue = 50, rangeValue = 48}
      ]
    , MapValue "soil" "fertilizer"
      [ MapRow {resetValue = 0, startValue = 15, rangeValue = 37}
      , MapRow {resetValue = 37, startValue = 52, rangeValue = 2}
      , MapRow {resetValue = 39, startValue = 0, rangeValue = 15}
      ]
    ]
  }
