{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Aoc2024.Day4
 where
import Data.Map qualified as Map
import Data.Maybe (catMaybes,isJust)
import Data.Map (Map)
import Control.Monad (mfilter)
import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as V

type GridCoord = (Int, Int)

{-# INLINE (.+.) #-}
(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

createGridMap :: [[a]] -> Map.Map GridCoord a
createGridMap v = Map.fromList [((x, y), c) | (y, row) <- zip [0..] v, (x, c) <- zip [0..] row]

prob1 ::  String-> Int
prob1 input=
  let inputGrid = createGridMap $ lines input
  in length 
      $ filter (== True) 
      $ concatMap (\xy -> map (matchWord "XMAS" inputGrid xy) directions) 
      $ Map.keys inputGrid  

matchWord :: String ->Map GridCoord Char -> GridCoord -> (Int, Int) -> Bool
matchWord word grid (x, y) (dx, dy)  =
    let coords = map (\i -> (x, y) .+. (i * dx, i * dy)) [0 ..]
        checkMatch (coord, c) = mfilter (==c) $ Map.lookup coord grid        
    in isJust $ mapM checkMatch $ zip coords word

directions :: [(Int, Int)]
directions = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

prob2 ::  String-> Int
prob2 input=
  let inputGrid = createGridMap $ lines input
  in length $ catMaybes $ map (findPattern'' inputGrid) $ Map.keys inputGrid

findPattern'' :: Map GridCoord Char -> GridCoord -> Maybe (Vector 5 Char)
findPattern'' grid coord =     
  mfilter (`elem` patterns)  $ V.mapM (\offset -> Map.lookup (coord .+. offset) grid) offsets
  where
    offsets = V.fromTuple ((0, 0), (2, 0), (1, 1), (0, 2), (2, 2)) :: Vector 5 (Int, Int)
    patterns = [V.fromTuple ('M', 'S', 'A', 'M', 'S'), 
                V.fromTuple ('M', 'M', 'A', 'S', 'S'), 
                V.fromTuple ('S', 'S', 'A', 'M', 'M'), 
                V.fromTuple ('S', 'M', 'A', 'S', 'M')]




matchWord' :: String ->Map GridCoord Char -> GridCoord -> (Int, Int) -> Bool
matchWord' wordToMatch grid (x, y) (dx, dy)  = 
    isJust $ mfilter (==wordToMatch) 
      $ mapM (\coord -> Map.lookup coord grid ) 
      $ zipWith (\i _ -> (x, y) .+. (i * dx, i * dy)) [0 ..]  wordToMatch
    

findPattern :: Map GridCoord Char -> GridCoord -> Maybe GridCoord
findPattern grid coord = do    
    spots <- V.mapM (\offset -> Map.lookup (coord .+. offset) grid) offsets
    if spots `elem` patterns
        then Just coord
        else Nothing
  where
    offsets = V.fromTuple ((0, 0), (2, 0), (1, 1), (0, 2), (2, 2)) :: Vector 5 (Int, Int)
    patterns = [V.fromTuple ('M', 'S', 'A', 'M', 'S'), 
                V.fromTuple ('M', 'M', 'A', 'S', 'S'), 
                V.fromTuple ('S', 'S', 'A', 'M', 'M'), 
                V.fromTuple ('S', 'M', 'A', 'S', 'M')]

findPattern' :: Map GridCoord Char -> GridCoord -> Maybe GridCoord
findPattern' grid coord@(x, y) = do
    spots <- (,,,,)
        <$> Map.lookup ((.+.) (x,y) (0,0)) grid
        <*> Map.lookup ((.+.) (x,y) (2,0)) grid
        <*> Map.lookup ((.+.) (x,y) (1,1)) grid
        <*> Map.lookup ((.+.) (x,y) (0,2)) grid
        <*> Map.lookup ((.+.) (x,y) (2,2)) grid
       
    if spots `elem` patterns
        then Just coord
        else Nothing
  where
    patterns = [('M', 'S', 'A', 'M', 'S'), --should compute this and coords
                ('M', 'M', 'A', 'S', 'S'), 
                ('S', 'S', 'A', 'M', 'M'), 
                ('S', 'M', 'A', 'S', 'M')]


xmaxOptions :: [[((Int,Int),(Int,Int))]]
xmaxOptions =[[x,y] | x <- [((0,0),(1,1)),((2,2),(-1,-1))], y <- [((0,0),(1,1)),((2,2),(-1,-1))]]

-- >>> xmaxOptions
-- [[((0,0),(1,1)),((0,0),(1,1))],[((0,0),(1,1)),((2,2),(-1,-1))],[((2,2),(-1,-1)),((0,0),(1,1))],[((2,2),(-1,-1)),((2,2),(-1,-1))]]

