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
import GHC.TypeLits (KnownNat)
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
      $ filter id
      $ concatMap (\xy -> map (matchWord "XMAS" inputGrid xy) directions) 
      $ Map.keys inputGrid  

matchWord :: String -> Map GridCoord Char -> GridCoord -> (Int, Int) -> Bool
matchWord word grid startCoord (dx, dy)  =
    let coords = map (\i -> startCoord .+. (i * dx, i * dy)) [0 ..]
        checkMatch (coord, c) = mfilter (==c) $ Map.lookup coord grid        
    in isJust $ mapM checkMatch $ zip coords word

directions :: [(Int, Int)]
directions = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

prob2 :: String -> Int
prob2 input=
  let inputGrid = createGridMap $ lines input
  in length $ catMaybes $ map (findPattern'' patterns offsets inputGrid) $ Map.keys inputGrid
  where
    patterns = [V.fromTuple ('M', 'S', 'A', 'M', 'S'), 
                V.fromTuple ('M', 'M', 'A', 'S', 'S'), 
                V.fromTuple ('S', 'S', 'A', 'M', 'M'), 
                V.fromTuple ('S', 'M', 'A', 'S', 'M')]
    offsets = V.fromTuple ((0, 0), (2, 0), (1, 1), (0, 2), (2, 2)) :: Vector 5 (Int, Int)

findPattern'' :: (KnownNat n, Eq a) => [Vector n a] -> Vector n (Int, Int) -> Map GridCoord a -> GridCoord -> Maybe (Vector n a)
findPattern'' patterns offsets grid coord =     
  let lookupOffset = (`Map.lookup` grid) . (coord .+. )
      spots = V.mapM lookupOffset offsets
  in mfilter (`elem` patterns) spots 