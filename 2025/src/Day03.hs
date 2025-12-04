module Day03 (parse, part1, part2) where

import Utils (split, splitLast)

type Day = [[Int]]

-- Parsing
parse :: String -> Day -- adjust type per puzzle
parse x = findBank <$> split '\n' x
  where
    findBank x' = read . (: []) <$> x'

needsShift :: [Int] -> ([Int], Bool)
needsShift (a : b : xs)
  | a < b = (b : xs, True)
  | otherwise = (a : xs', o)
  where
    (xs', o) = needsShift (b : xs)
needsShift x = (x, False)

findBestJoltage :: [Int] -> Int -> [Int]
findBestJoltage xs x
  | shifts = xs' ++ [x]
  | l < x = xs'' ++ [x]
  | otherwise = xs'
  where
    (xs', shifts) = needsShift xs
    (xs'', l) = splitLast xs'

totalJoltage :: [Int] -> Int
totalJoltage = foldl (\a -> (+) (a * 10)) 0

-- Part 1
part1 :: Day -> Int
part1 xs = sum $ totalJoltage . foldl findBestJoltage [0, 0] <$> xs -- placeholder

-- Part 2
part2 :: Day -> Int
part2 xs = sum $ totalJoltage . foldl findBestJoltage (replicate 12 0) <$> xs -- placeholder
