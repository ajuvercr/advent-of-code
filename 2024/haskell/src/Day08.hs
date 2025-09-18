{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day08 (parseDay, part1, part2) where

import Control.Monad.State
import Data.Functor
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils (pairs)

type Coord = (Int, Int)

type FreqMap = Map.Map Char [Coord]

step :: Char -> Int -> Int -> State FreqMap Char
step '.' _ _ = pure '.'
step c k j = modify update $> c
  where
    update = Map.insertWith (++) c [(k, j)]

steps :: Int -> Int -> String -> State FreqMap ()
steps _ j ('\n' : xs) = steps 0 (j + 1) xs
steps i j (x : xs) = step x i j >> steps (i + 1) j xs
steps _ _ [] = pure ()

split :: Char -> String -> [String]
split char = _split char []
  where
    _split _ [] [] = []
    _split _ x [] = [x]
    _split c ys (x : xs) | c == x = ys : _split c [] xs
    _split c ys (x : xs) = _split c (x : ys) xs

parseDay :: String -> (FreqMap, Coord) -- adjust type per puzzle
parseDay inp = (execState (steps 0 0 inp) Map.empty, bound)
  where
    splits = split '\n' inp
    bound = (count splits, count $ head splits)

addAntinodes :: Set.Set Coord -> (Coord, Coord) -> Set.Set Coord
addAntinodes coords ((x1, y1), (x2, y2)) = Set.insert one $ Set.insert two coords
  where
    (dx, dy) = (x1 - x2, y1 - y2)
    one = (x1 + dx, y1 + dy)
    two = (x2 - dx, y2 - dy)

addAntinodes2 :: Coord -> Set.Set Coord -> (Coord, Coord) -> Set.Set Coord
addAntinodes2 bound coords ((x1, y1), (x2, y2)) = foldl (flip Set.insert) coords antinodes
  where
    (dx, dy) = (x1 - x2, y1 - y2)
    lower = max (max x1 x2 `div` dx) (max y1 y2 `div` dy) + 1
    upper = max (fst bound `div` dx) (snd bound `div` dy) + 1
    antinodes = map getNode [-1 * lower .. upper]
    getNode i = (x1 + i * dx, y1 + i * dy)

count :: [a] -> Int
count = foldr (\_ -> (+) 1) 0

valid :: Coord -> Coord -> Bool
valid (mx, my) (x, y) = x >= 0 && y >= 0 && x < my && y < mx

-- Part 2
part1 :: (FreqMap, Coord) -> Int
part1 (freqMap, (mx, my)) = count valids
  where
    valids = filter (valid (mx, my)) (Set.toList antinodeSet)
    antinodeSet = foldl addAntinodes Set.empty allPairs
    allPairs = concatMap pairs elems
    elems = Map.elems freqMap

-- Part 2
part2 :: (FreqMap, Coord) -> Int
part2 (freqMap, (mx, my)) = count valids
  where
    valids = filter (valid (mx, my)) (Set.toList antinodeSet)
    antinodeSet = foldl (addAntinodes2 (mx, my)) Set.empty allPairs
    allPairs = concatMap pairs elems
    elems = Map.elems freqMap
