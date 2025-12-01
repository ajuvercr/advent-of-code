{-# LANGUAGE TupleSections #-}

module Day10 (parseDay, part1, part2) where

import Control.Monad.State
import qualified Data.Map as Map

type Coord = (Int, Int)

type Day = Map.Map Coord Int

toMap :: (Int, Int) -> [[Int]] -> State Day ()
toMap _ [] = pure ()
toMap (_, y) ([] : xs) = toMap (0, y + 1) xs
toMap (i, j) ((y : ys) : xs) = modify (Map.insert (i, j) y) >> toMap (i + 1, j) (ys : xs)

parseDay :: String -> Day -- adjust type per puzzle
parseDay inp = out
  where
    out = execState (toMap (0, 0) heights) Map.empty
    heights = map (map (read . (: []))) (lines inp)

starts :: Day -> [Coord]
starts = Map.keys . Map.filter (== 0)

adjecent :: Coord -> Int -> [(Coord, Int)]
adjecent (x, y) i = map (,i) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

findHeights :: Day -> Int -> Map.Map Coord Int -> Map.Map Coord Int
findHeights _ 9 x = x
findHeights heights h coords = findHeights heights (h + 1) nextLocations
  where
    nextLocations = foldl updateThis Map.empty $ filter isGood options
    updateThis m (coord, count) = Map.insertWith (+) coord count m
    options = concatMap (uncurry adjecent) (Map.toList coords)
    isGood (x, _) = heights Map.!? x == Just (h + 1)

findHeightsFrom :: Day -> Coord -> Map.Map Coord Int
findHeightsFrom heights coord = findHeights heights 0 $ Map.singleton coord 1

-- Part 2
part1 :: Day -> Int
part1 heights = sum $ map length items
  where
    items = map (findHeightsFrom heights) (starts heights)

-- Part 2
part2 :: Day -> Int
part2 heights = vs
  where
    vs = sum $ concatMap Map.elems items
    items = map (findHeightsFrom heights) (starts heights)
