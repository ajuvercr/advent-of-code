module Day02 (parse, part1, part2) where

import Utils (chunksOf, split)

type Day = [(Int, Int)]

firstTwo :: [a] -> (a, a)
firstTwo [a, b] = (a, b)
firstTwo _ = error "expected two parts"

-- Parsing
parse :: String -> Day -- adjust type per puzzle
parse x = findRange <$> split ',' x
  where
    findRange x' = firstTwo $ read <$> split '-' x'

isInvalid :: Int -> Bool
isInvalid x = a == b
  where
    st = show x
    l = length st
    (a, b) = splitAt (l `div` 2) st

isInvalid' :: Int -> Bool
isInvalid' x = any played l
  where
    st = show x
    l = chunks <$> [1 .. length st `div` 2]
    chunks i = chunksOf i st
    played :: [String] -> Bool
    played [] = True
    played [_] = True
    played (a : b : xs) = a == b && played (b : xs)

countInvalids :: (Int -> Bool) -> (Int, Int) -> Int
countInvalids invalid (start, end) = sum $ invalid `filter` [start .. end]

-- Part 1
part1 :: Day -> Int
part1 xs = sum $ countInvalids isInvalid <$> xs -- placeholder

-- Part 2
part2 :: Day -> Int
part2 xs = sum $ countInvalids isInvalid' <$> xs -- placeholder
