module Spec.Aoc2021.Common
  ( Day (..),
    PuzzleInput (..),
    readPuzzleInput,
  )
where

import Data.Functor ((<&>))
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))

data Day = Day3 | Day5 deriving (Show)

data PuzzleInput
  = PuzzleExample1
  | Puzzle
  deriving (Show)

baseDir :: IO FilePath
baseDir = getCurrentDirectory <&> (</> "Data" </> "Aoc2021")

readPuzzleInput :: Day -> PuzzleInput -> IO String
readPuzzleInput d p = do
  baseD <- baseDir
  let file = baseD </> show d <> "_" <> show p <> ".txt"
  readFile file