module Day05 (parse, part1, part2) where

import Data.List (sort)
import NanoParsec (Parser, char, number, runParser, star)

type Day = ([(Int, Int)], [Int])

-- Parsing
parse :: String -> Day -- adjust type per puzzle
parse = runParser parser
  where
    parser = (,) <$> star parseRange <* char '\n' <*> star parseInventory
    parseRange :: Parser (Int, Int)
    parseRange = do
      start <- number <* char '-'
      end <- number <* char '\n'
      return (start, end + 1)
    parseInventory :: Parser Int
    parseInventory = number <* char '\n'

-- Part 1
part1 :: Day -> Int
part1 (ranges, invent) = length $ isFresh `filter` invent
  where
    isFresh i = any (fresh i) ranges
    fresh i (start, end) = i >= start && i < end

countRange :: [(Int, Int)] -> Int
countRange [] = 0
countRange [(s, e)] = e - s
countRange ((s, e) : (x, y) : xs)
  | x >= s && x < e = countRange ((s, max e y) : xs)
  | otherwise = e - s + countRange ((x, y) : xs)

-- Part 2
part2 :: Day -> Int
part2 (f, _) = countRange $ sort f
