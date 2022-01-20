module Spec.Aoc2021.Common 
(
    Day(..),
    PuzzleInput(..),
    readInput
)

where

import Data.Functor ( (<&>) )
import System.Directory ( getCurrentDirectory )
import System.FilePath.Posix ( (</>) )

data Day = Day5 deriving Show

data PuzzleInput = PuzzleExample1
                 | Puzzle deriving Show

baseDir :: IO FilePath
baseDir = getCurrentDirectory <&>  (</> "Data" </> "Aoc2021")

readInput :: Day -> PuzzleInput -> IO String
readInput d p = do
    baseD <- baseDir
    let file = baseD </> show d <> "_" <> show p <> ".txt"    
    readFile file