module Spec.Common
  (  AocYear(..),
    PuzzleInput (..),
    readPuzzleInput,
  )
where

import Data.Functor ((<&>))
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))

data PuzzleInput
  = PuzzleExample1
  | Puzzle
  deriving (Show)

data AocYear
  = Aoc2020
  | Aoc2021
  deriving (Show)

baseDir :: Show y => y -> IO FilePath
baseDir y = getCurrentDirectory <&> (</> "Data" </> show y)

readPuzzleInput :: (Show y, Show d) => y ->  d -> PuzzleInput -> IO String
readPuzzleInput y d p = do
  baseD <- baseDir y
  let file = baseD </> show d <> "_" <> show p <> ".txt"
  readFile file