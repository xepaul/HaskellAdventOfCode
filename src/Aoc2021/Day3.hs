{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS -Wunused-imports #-}
-- {-# OPTIONS -Wunused- #-}

module Aoc2021.Day3 where

import Control.Lens (makeLenses, (&), (+~))
import Control.Monad (Monad ((>>=)))
import Data.Char (intToDigit)
import Data.Either (Either (..))
import Data.Either.Extra (mapLeft)
import Data.Eq (Eq)
import Data.Foldable (Foldable (foldl))
import Data.Functor (($>))
import Data.List (filter, foldr, map, reverse, transpose, unfoldr, (!!))
import Data.Maybe (Maybe (..))
import GHC.Base (Applicative (liftA2))
import GHC.Show (Show (show))
import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
  ( char,
    choice,
    many1,
    sepEndBy1,
  )
import Prelude
  ( Char,
    Int,
    String,
    div,
    flip,
    rem,
    return,
    ($),
    (*),
    (+),
    (.),
    (<),
    (<$>),
    (==),
    (>),
    (>=),
  )

charBin2dec :: String -> Int
charBin2dec = bin2dec c2i
  where
    c2i c = if c == '0' then 0 else 1

bitsBin2dec :: [Bit] -> Int
bitsBin2dec = bin2dec \case
  One -> 1
  Zero -> 0

bin2dec :: (a -> Int) -> [a] -> Int
bin2dec c2i = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i

dec2bin :: Int -> String
dec2bin = reverse . map intToDigit . unfoldr (\x -> if x == 0 then Nothing else Just (rem x 2, div x 2))

-- >>>  bin2dec "1001"
-- 9
-- >>>  bin2dec "0111"
-- 7

data Bit = One | Zero deriving (Show, Eq)

negateBit :: Bit -> Bit
negateBit = \case
  One -> Zero
  Zero -> One

data BitsFreqCount = BitsFreqCount {_ones :: Int, _zeros :: Int}

makeLenses ''BitsFreqCount

parseLines :: String -> Either String [[Bit]]
parseLines = mapLeft show . parse inputParser ""
  where
    inputParser = sepEndBy1 parseLine (char '\n')
    parseLine = many1 (choice [pZeroParser, pOneParser])
    pZeroParser = char '0' $> Zero
    pOneParser = char '1' $> One

-- >>> parseLines "111\n101 "
-- Right [[One,One,One],[One,Zero,One]]

day3Part1FromInput :: String -> Either String Int
day3Part1FromInput l = day3Part1 <$> parseLines l

countBits :: [Bit] -> BitsFreqCount
countBits =
  foldl
    ( \s -> \case
        One -> s & ones +~ 1
        Zero -> s & zeros +~ 1
    )
    (BitsFreqCount 0 0)

day3Part1 :: [[Bit]] -> Int
day3Part1 content =
  let t = transpose content
      collaspeColumnsOfBinBy :: (BitsFreqCount -> Bit) -> Int
      collaspeColumnsOfBinBy f =
        bitsBin2dec $
          map (f . countBits) t
      gammaBinary = collaspeColumnsOfBinBy \(BitsFreqCount onesCnt zerosCnt) ->
        if onesCnt > zerosCnt then One else Zero

      epsilonBinary = collaspeColumnsOfBinBy \(BitsFreqCount onesCnt zerosCnt) ->
        if onesCnt < zerosCnt then One else Zero
   in (gammaBinary * epsilonBinary)

day3Part2FromInput :: String -> Either String Int
day3Part2FromInput l = parseLines l >>= day3Part2

day3Part2 :: [[Bit]] -> Either [Char] Int
day3Part2 contentLines =
  let gammaRate = processRating3 oxygenRatingSelector contentLines
      epsilonRate = processRating3 c02RatingSelector contentLines
   in liftA2 (*) gammaRate epsilonRate
  where
    getIndex = flip (!!)

    oxygenRatingSelector :: [Bit] -> Bit
    oxygenRatingSelector = (\(BitsFreqCount onesCnt zerosCnt) -> if onesCnt >= zerosCnt then One else Zero) . countBits

    c02RatingSelector :: [Bit] -> Bit
    c02RatingSelector = (\(BitsFreqCount onesCnt zerosCnt) -> if onesCnt < zerosCnt then One else Zero) . countBits

    processRating3 :: ([Bit] -> Bit) -> [[Bit]] -> Either [Char] Int
    processRating3 selector xs =
      bitsBin2dec
        <$> processRatingRec 0 xs
      where
        processRatingRec :: Int -> [[Bit]] -> Either [Char] [Bit]
        processRatingRec _ [] = Left "no rows"
        processRatingRec _ [x] = return x
        processRatingRec col rows = do
          let criteria =
                ( getIndex col
                    . map selector
                    . transpose
                )
                  rows
          processRatingRec (col + 1) $ filter (\row -> row !! col == criteria) rows