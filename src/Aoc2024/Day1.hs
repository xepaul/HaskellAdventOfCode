module Aoc2024.Day1
 where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (sort)
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Either.Extra (mapLeft)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)



-- Define a parser for a single tuple
tupleParser :: Parser (Int, Int)
tupleParser = do
  x <- many1 digit
  spaces
  y <- many1 digit
  return (read x, read y)

-- Define a parser for multiple tuples
tuplesParser :: Parser [(Int, Int)]
tuplesParser = tupleParser `sepEndBy` newline

-- Function to parse the input string
parseTuples :: String -> Either String [(Int, Int)]
parseTuples =  mapLeft show . parse tuplesParser ""

-- Example usage
exampleInput :: String
exampleInput = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"


-- >>> parseTuples exampleInput
-- Right [(3,4),(4,3),(2,5),(1,3),(3,9),(3,3)]


difference :: Num a => a -> a -> a
difference x y = abs (x - y)

-- >>> difference 3 4


prop1 :: [(Int, Int)] -> Int
prop1 i = let (a,b)= foldr f ([], []) i
              (x,y) = (sort a,sort b)
              distances = zipWith difference x y
          in sum distances 
  where
    f (x, y) (xs, ys) = (x : xs, y : ys)


prop1a' :: [(Int, Int)] -> Int
prop1a' i = sum $ zipWith difference (sort a) (sort b)
  where
    (a, b) = unzip i


prop1a''' :: [(Int, Int)] -> Int
prop1a''' = sum 
            . uncurry (zipWith difference) 
            . (\(a, b) -> (sort a, sort b)) 
            . unzip

prop1a'' :: [(Int, Int)] -> Int
prop1a'' = unzip
            >>> (\(a, b) -> (sort a, sort b))
            >>> uncurry (zipWith difference)
            >>> sum


prop1a'''' :: [(Int, Int)] -> Int
prop1a'''' i = i
                & unzip
                & \(a, b) -> (sort a, sort b)
                & uncurry (zipWith difference)
                & sum

-- >>> prop1 [(3,4),(4,3),(2,5),(1,3),(3,9),(3,3)]
-- 11


prob2 :: [(Int, Int)] -> Int
prob2 = unzip
            >>> (\(a, b) -> (a, getLookup b))
            >>> uncurry (\l f ->  map (\a -> a * f a) l)
            >>> sum
  where
    countOccurrences :: (Ord a) => [a] -> Map a Int
    countOccurrences = foldr (\x -> Map.insertWith (+) x 1) Map.empty

    getLookup :: (Ord a) => [a] -> (a -> Int)
    getLookup xs = let m = countOccurrences xs
                    in  fromMaybe 0 . flip Map.lookup m 


prob2_1 :: [(Int, Int)] -> Int
prob2_1 = unzip
            >>> (\(a, b) -> (a, getLookup b))
            >>> uncurry (\l f ->  map (\a -> a * f a) l)
            >>> sum
  where
    countOccurrences :: (Ord a) => [a] -> Map a Int
    countOccurrences = foldr (\x -> Map.insertWith (+) x 1) Map.empty

    getLookup :: (Ord a) => [a] -> (a -> Int)
    getLookup xs = let m = countOccurrences xs
                    in  fromMaybe 0 . flip Map.lookup m 