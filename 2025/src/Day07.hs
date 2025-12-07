{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day07 (parseDay, part1, part2) where

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, empty, insert, intersection, member, singleton, toDescList, toList, union)
import Utils (nTimes)

type Coord = (Int, Int)

type Day = (Coord, Set Coord, Int)

-- Parsing
parseDay :: String -> Day
parseDay xs' = (s, r, length $ lines xs')
  where
    (s, r) = getSet (0, 0) xs'
    getSet :: Coord -> String -> (Coord, Set Coord)
    getSet (i, j) ('^' : xs) = (s, (j, i) `insert` r)
      where
        (s, r) = getSet (i + 1, j) xs
    getSet (i, j) ('S' : xs) = ((j, i), r)
      where
        (_, r) = getSet (i + 1, j) xs
    getSet (_, j) ('\n' : xs) = getSet (0, j + 1) xs
    getSet (i, j) (_ : xs) = getSet (i + 1, j) xs
    getSet _ [] = ((0, 0), empty)

moveDown :: Set Coord -> (Set Coord, Int) -> (Set Coord, Int)
moveDown splitter (xs, times) = (newSet, times + extra)
  where
    newSet = foldl union empty $ moveDown' <$> toList xs
    extra = length $ intersection xs splitter
    moveDown' :: Coord -> Set Coord
    moveDown' x@(i, j)
      | x `member` splitter = (i + 1, j - 1) `insert` singleton (i + 1, j + 1)
      | otherwise = singleton (i + 1, j)

type Tree = Map Coord [Coord]

buildTree :: Int -> [Coord] -> Tree -> Tree
buildTree _ [] x = x
buildTree l (x@(i, j) : xs) tree = ts
  where
    rest = buildTree l xs tree
    left = find (`Map.member` tree) $ (,j - 1) <$> [i + 1 .. i + l]
    right = find (`Map.member` tree) $ (,j + 1) <$> [i + 1 .. i + l]
    mcons :: Maybe Coord -> Tree -> Tree
    mcons (Just k) tree' = Map.update (update k) x tree'
    mcons Nothing tree' = tree'
    update x' xs' = Just (x' : xs')
    ts = mcons left $ mcons right rest

countTree :: Tree -> [Coord] -> Map Coord Int -> Map Coord Int
countTree _ [] counts = counts
countTree tree (x : xs) counts = countTree tree xs (Map.insert x total counts)
  where
    children = tree Map.! x
    childSum = (counts Map.!) <$> children
    total = counts Map.! x + sum childSum

-- Part 1
part1 :: Day -> Int
part1 (s, r, l) = snd $ nTimes l (moveDown r) (singleton s, 0)

-- Part 2
part2 (_, r, l) = h
  where
    countStart = Map.map m2 tree
    counted = countTree tree (toDescList r) countStart
    h = (snd . head . Map.toList) counted
    m2 x = 2 - length x
    tree = buildTree l (toList r) (Map.fromList $ (,[]) <$> toList r)
