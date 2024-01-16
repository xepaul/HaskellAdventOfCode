module Spec.Common
  ( AocYear (..),
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

-- readPuzzleInput reads the puzzle input for day d of year y.
-- It returns a string with the contents of the file.


readPuzzleInput :: (Show y, Show d) => y ->  d -> PuzzleInput -> IO String
readPuzzleInput y d p = do
  baseD <- baseDir y
  let file = baseD </> show d <> "_" <> show p <> ".txt"
  readFile file
readPuzzleInput2021 :: Int -> PuzzleInput -> IO String
readPuzzleInput2021 = readPuzzleInput Aoc2021
