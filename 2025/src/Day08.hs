module Day08 (parseDay, part1, part2) where

import Data.List (sort, sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Utils (nTimes, split)

type Coord = (Int, Int, Int)

type Day = (Map.Map Coord Int, Closest)

type Closest = [(Int, Coord, Coord)]

dist :: Coord -> Coord -> Int
dist (a1, b1, c1) (a2, b2, c2) = da * da + db * db + dc * dc
  where
    da = a1 - a2
    db = b1 - b2
    dc = c1 - c2

parseDay :: String -> Day
parseDay inp = (coordMap, sort $ findDists `concatMap` coords)
  where
    coordMap = Map.fromList $ zip coords [0 ..]
    findDist a b
      | a < b = [(dist a b, a, b)]
      | otherwise = []
    findDists a = findDist a `concatMap` coords
    coords = toTriples . (read <$>) . split ',' <$> lines inp
    toTriples [a, b, c] = (a, b, c)
    toTriples _ = error "not a triple"

combine' :: (Map.Map Coord Int, Closest) -> (Map.Map Coord Int, Closest)
combine' (map', (_, a, b) : xs)
  | ac == bc = (map', xs)
  | otherwise = (newMap, xs)
  where
    ac = map' Map.! a
    bc = map' Map.! b
    newMap = mapValue <$> map'
    mapValue v
      | v == ac = bc
      | otherwise = v
combine' _ = error "nah"

countSizes :: Map.Map Coord Int -> [Int]
countSizes map' = sortBy (comparing (* (-1))) counts
  where
    counts = Map.elems $ foldr (Map.alter alter) Map.empty map'
    alter (Just x) = Just (x + 1)
    alter Nothing = Just 1

output :: [Int] -> Int
output (a : b : c : _) = a * b * c
output _ = error "nah"

untilOutput :: (a -> Bool) -> (a -> a) -> a -> a
untilOutput f a i
  | f next = i
  | otherwise = untilOutput f a next
  where
    next = a i

-- Part 2
part1 :: Day -> Int
part1 (y, x) = output $ countSizes combined
  where
    (combined, _) = nTimes 1000 combine' (y, x)

output2 :: Closest -> Int
output2 ((_, (x, _, _), (y, _, _)) : _) = x * y
output2 _ = error "nah"

-- Part 2
part2 :: Day -> Int
part2 (y, x) = output2 xs
  where
    (_, xs) = untilOutput allSame combine' (y, x)
    allSame (m, _) = length (Set.fromList $ Map.elems m) == 1
