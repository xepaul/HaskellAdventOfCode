module Aoc2023.Day1_2 where

import Data.Char (isDigit)

collectDigits :: String -> [String]
collectDigits [] = []
collectDigits (x : xs)
  | isDigit x =
      let (digits, rest) = span isDigit (x : xs)
       in digits : collectDigits rest
  | otherwise = collectDigits xs

prob1 :: String -> Int
prob1 = sum . fmap (read . keepfirstAndLAst . collectDigits) . lines
  where
    keepfirstAndLAst :: [String] -> String
    keepfirstAndLAst x = head x ++ last x

    

exampleInput = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

-- >>> hdd exampleInput
-- 142
