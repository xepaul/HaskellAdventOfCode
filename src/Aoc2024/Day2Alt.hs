{-# LANGUAGE BangPatterns #-}
module Aoc2024.Day2Alt
 where

import Text.Parsec ( char, digit, sepEndBy, many1, parse )
import Text.Parsec.String (Parser)
import Data.Either.Extra (mapLeft)
import Control.Monad.Combinators (sepBy1)
import Control.Monad (foldM)

reportsParser :: Parser [[Int]]
reportsParser = parseLine `sepEndBy` (char '\n')
  where
    parseLine =  many1 (digit) `sepBy1` (char ' ') >>= return . map read;

simpleReportsParser :: String -> Either String [[Int]]
simpleReportsParser =  mapLeft show . parse reportsParser ""

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