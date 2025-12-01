module Day01 (parse, part1, part2) where

-- Parsing
parse :: String -> [(Char, Int)] -- adjust type per puzzle
parse = map readOne . lines
  where
    readOne (x : xs) = (x, read xs)
    readOne [] = ('0', 0)

turn :: (Int, Int, Int) -> (Char, Int) -> (Int, Int, Int)
turn (at, tot, tot2) ('L', amount)
  | new `mod` 100 == 0 = (new, tot + 1, newTot2 + 1)
  | otherwise = (new, tot, newTot2)
  where
    new = at - amount
    startC
      | at `mod` 100 == 0 = (at - 1) `div` 100
      | otherwise = at `div` 100
    endC = new `div` 100
    newTot2 = tot2 + abs (endC - startC)
turn (at, tot, tot2) ('R', amount)
  | new `mod` 100 == 0 = (new, tot + 1, newTot2)
  | otherwise = (new, tot, newTot2)
  where
    new = at + amount
    startC = at `div` 100
    endC = new `div` 100
    newTot2 = tot2 + abs (endC - startC)
turn _ _ = error "empty list"

-- Part 1
-- Not 4510
part1 :: [(Char, Int)] -> Int
part1 xs = t
  where
    (_, t, _) = foldl turn (50, 0, 0) xs

part2 :: [(Char, Int)] -> Int
part2 xs = t
  where
    (_, _, t) = foldl turn (50, 0, 0) xs
