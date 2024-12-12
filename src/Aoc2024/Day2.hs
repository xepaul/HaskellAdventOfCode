module Aoc2024.Day2
 where

import Text.Parsec hiding (optional, sepBy1)
import Text.Parsec.String (Parser)
import Data.List (sort)
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Either.Extra (mapLeft)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Control.Monad.Combinators (sepBy1)
import Debug.Trace (trace)
import Control.Monad (foldM)


-- Define a parser for a single tuple
integer :: Parser Int
integer = read <$> many1 digit

-- Define a parser for multiple tuples
integerList :: Parser [Int]
integerList = integer `sepBy` spaces

ff:: Parser [Int]
ff =   sepBy1 (read <$> many1 digit) (char ' ')
reportsParser :: Parser [[Int]]
reportsParser = ff `sepEndBy` (char '\n')

-- Function to parse the input string
simpleReportsParser :: String -> Either String [[Int]]
simpleReportsParser =  mapLeft show . parse reportsParser ""

-- Example usage
exampleInput :: String
exampleInput = "3 4\n7 9\n8 9"


type Level = Int
data LevelProgression = LevelProgressionIncrease Level  
                      | LevelProgressionDecrease Level
                      | LevelProgressionUnknown Level
                      | LevelProgressionStarting 
                      deriving (Show,Eq)

data ReportStatus = ReportStatusSafe | ReportStatusUnsafe deriving (Show,Eq)


prob1_a :: [Level] -> ReportStatus
prob1_a z = 
  let xx = foldM step LevelProgressionStarting z
  in case xx of 
       Left _  -> ReportStatusUnsafe
       Right _ -> ReportStatusSafe
  where
    step :: LevelProgression -> Level -> Either Int LevelProgression
    step LevelProgressionStarting x = Right $ LevelProgressionUnknown x
    step (LevelProgressionUnknown oldLevel) x = 
      let dif = abs (x - oldLevel)
      in if dif < 1 || dif > 3
         then Left x
         else if x > oldLevel
              then Right $ LevelProgressionDecrease x
              else Right $ LevelProgressionIncrease x
    step (LevelProgressionIncrease oldLevel) x = 
      let dif = x - oldLevel
      in if x > oldLevel && dif >= 1 && dif <= 3
         then Right $ LevelProgressionIncrease x
         else Left x
    step (LevelProgressionDecrease oldLevel) x = 
      let dif = oldLevel - x
      in if x < oldLevel && dif >= 1 && dif <= 3
         then Right $ LevelProgressionDecrease x
         else Left x


data LevelProgression2 = LevelProgressionWithRequired (Level->Level->Bool) Level
                      | LevelProgression2Unknown Level
                      | LevelProgression2Starting 


prob1_b :: [Level] -> ReportStatus
prob1_b z = 
  let xx = foldM step LevelProgression2Starting z
  in case xx of 
       Left _  -> ReportStatusUnsafe
       Right _ -> ReportStatusSafe
  where
    step :: LevelProgression2 -> Level -> Either Int LevelProgression2
    step LevelProgression2Starting x = Right $ LevelProgression2Unknown x
    step (LevelProgression2Unknown oldLevel) x = 
      if (not ( isInRange x oldLevel)) 
         then Left x
         else if x > oldLevel
              then Right $ LevelProgressionWithRequired (>) x
              else Right $ LevelProgressionWithRequired (<) x
    step (LevelProgressionWithRequired f oldLevel) x = 
      if f oldLevel x && isInRange oldLevel x
         then Right $ LevelProgressionWithRequired f x
         else Left x
    isInRange :: Level -> Level -> Bool
    isInRange x level = 
      let dif = abs (x - level)
      in dif >= 1 && dif <= 3

data ReportStatus2 = ReportStatusSafe2 | ReportStatusUnsafe2 String deriving (Show,Eq)

prob1 :: [[Int]] -> Int
prob1 =  length . filter (== ReportStatusSafe2) . map (go Nothing )
  where  
    go :: Maybe Level -> [Int] -> ReportStatus2
    go _ [] = ReportStatusSafe2
    go Nothing (x:xs) = go (Just x) xs
    go (Just level) (x:xs) 
        | (not ( isInRange x level)) = ReportStatusUnsafe2 "a"
        | otherwise                  = let c = (if x > level then (>) else (<)) in checkLevelsProgression c x xs
    checkLevelsProgression :: (Int -> Int -> Bool) -> Level -> [Int] -> ReportStatus2
    checkLevelsProgression _ _ [] = ReportStatusSafe2 
    checkLevelsProgression f level (x:xs) 
      | f x level && isInRange x level = checkLevelsProgression f x xs
      | otherwise                      = ReportStatusUnsafe2 "b"
    isInRange :: Level -> Level -> Bool
    isInRange x level = 
      let dif = abs (x - level)
      in dif >= 1 && dif <= 3

ex1 = [7,6,4,2,1] :: [Int]
ex2 = [1,2,7,8,9] ::[Int]

-- >>> rec2 ex1
-- >>> rec2 ex2
-- ReportStatusSafe2
-- ReportStatusUnsafe2 "b"

-- >>> countOccurrences ex1
-- ReportStatusUnsafe

