
module Day02 (parse, part1, part2) where

-- Parsing
parse :: String -> [Int]  -- adjust type per puzzle
parse = map read . lines

-- Part 1
part1 :: [Int] -> Int
part1 xs = sum xs  -- placeholder

-- Part 2
part2 :: [Int] -> Int
part2 xs = product xs  -- placeholder
