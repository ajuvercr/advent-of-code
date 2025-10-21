module Day25 (parseDay, part1, part2) where

import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))

type Key = [Int]

type Day = ([Key], [Key])

splitKeys :: String -> NonEmpty String
splitKeys [] = [] :| []
splitKeys "\n" = [] :| []
splitKeys ('\n' : '\n' : xs) = [] <| splitKeys xs
splitKeys (x : xs) = (x : x') :| xs'
  where
    (x' :| xs') = splitKeys xs

addKey :: String -> Day -> Day
addKey [] x = x
addKey inp@(f : _) (x, y)
  | f == '#' = (key : x, y)
  | otherwise = (x, key : y)
  where
    key = length . filter (== '#') <$> transpose (lines inp)

parseDay :: String -> Day
parseDay x = foldr addKey ([], []) $ splitKeys x

fits :: Key -> Key -> Bool
fits a b = eqList added
  where
    addOne (x, y) = x + y
    added :: [Int]
    added = addOne <$> zip a b
    eqList :: [Int] -> Bool
    eqList = all (< 8)

part1 :: Day -> Int
part1 (a, b) = sum $ c <$> a
  where
    c x = length $ filter id $ map (fits x) b

-- Part 2
part2 :: Day -> ()
part2 _ = ()
