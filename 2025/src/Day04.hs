module Day04 (parse, part1, part2) where

import Data.Set (Set, difference, empty, fromList, insert, member, toList)

type Coord = (Int, Int)

type Day = Set Coord

addCoord :: Coord -> Coord -> Coord
addCoord (i1, j1) (i2, j2) = (i1 + i2, j1 + j2)

-- Parsing
parse :: String -> Day -- adjust type per puzzle
parse = getSet (0, 0)
  where
    getSet :: Coord -> String -> Set Coord
    getSet (i, j) ('@' : xs) = (i, j) `insert` getSet (i + 1, j) xs
    getSet (i, j) ('\n' : xs) = getSet (0, j + 1) xs
    getSet (i, j) (_ : xs) = getSet (i + 1, j) xs
    getSet _ [] = empty

getNeighbours :: Coord -> [Coord]
getNeighbours x = addCoord x <$> [(i, j) | i <- [-1 .. 1], j <- [-1 .. 1], i /= 0 || j /= 0]

getValids :: Day -> Day
getValids xs = fromList valids
  where
    valid x = 4 > length ((`member` xs) `filter` getNeighbours x)
    valids = valid `filter` toList xs

removeValids :: Day -> Day
removeValids xs = xs `difference` getValids xs

removeAllValids :: Day -> Day
removeAllValids xs
  | new == xs = xs
  | otherwise = removeAllValids new
  where
    new = removeValids xs

-- Part 1
part1 :: Day -> Int
part1 xs = length $ getValids xs

-- Part 2
part2 :: Day -> Int
part2 xs = length xs - length removed
  where
    removed = removeAllValids xs
