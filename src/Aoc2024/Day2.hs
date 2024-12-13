{-# LANGUAGE BangPatterns #-}
module Aoc2024.Day2
 where
import Text.Parsec ( char, digit, sepEndBy, many1, parse )
import Text.Parsec.String (Parser)
import Data.Either.Extra (mapLeft)
import Control.Monad.Combinators (sepBy1)
import Data.List.Index (deleteAt)

reportsParser :: Parser [[Int]]
reportsParser = parseLine `sepEndBy` (char '\n')
  where
    parseLine =  many1 (digit) `sepBy1` (char ' ') >>= return . map read;

simpleReportsParser :: String -> Either String [[Int]]
simpleReportsParser =  mapLeft show . parse reportsParser ""

type Level = Int

data ReportStatus = ReportStatusSafe | ReportStatusUnsafe String deriving (Show,Eq)

prob1 :: [[Int]] -> Int
prob1 =  length . filter (== ReportStatusSafe) . map (go1 Nothing )

prob2 :: [[Int]] -> Int
prob2 =  length . filter check1error 
  where  
    getPossibleReports :: [Int] -> [[Int]]
    getPossibleReports a = map (\i -> deleteAt  i a  )  [0 .. (length a) ]
    check1error :: [Int] -> Bool
    check1error = any ((== ReportStatusSafe) . go1 Nothing) . getPossibleReports

go1 :: Maybe Level -> [Int] -> ReportStatus
go1 _ [] = ReportStatusSafe
go1 Nothing (x:xs) = go1 (Just x) xs
go1 (Just level) (x:xs) 
    | (not ( isInRange x level)) = ReportStatusUnsafe "a"
    | otherwise                  = let c = (if x > level then (>) else (<)) in checkLevelsProgression c x xs      
checkLevelsProgression :: (Int -> Int -> Bool) -> Level -> [Int] -> ReportStatus
checkLevelsProgression _ _ [] = ReportStatusSafe
checkLevelsProgression f level (x:xs) 
  | f x level && isInRange x level = checkLevelsProgression f x xs
  | otherwise                      = ReportStatusUnsafe "b"
isInRange :: Level -> Level -> Bool
isInRange x level = 
  let dif = abs (x - level)
  in dif >= 1 && dif <= 3