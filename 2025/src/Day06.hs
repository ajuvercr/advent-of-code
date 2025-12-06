module Day06 (parseDay, part1, part2) where

import Control.Applicative ((<|>))
import Data.List (transpose)
import NanoParsec (char, number, oneOf, runParser, star)

data Chunk = Int Int | Char Char deriving (Show, Eq, Ord)

type Day = [[Chunk]]

parseDay :: String -> (Day, Day)
parseDay st = (transpose $ runParser parseLines st, runParser parseLines transposed)
  where
    chunk = Char <$> oneOf "+*" <|> Int <$> number
    line = spaces *> star (chunk <* spaces)
    spaces = star $ char ' '
    parseLines = star (line <* char '\n')
    transposed = unlines $ transpose $ lines st

calculate :: [Chunk] -> Int
calculate chunks
  | l == Char '*' = product numbers
  | l == Char '+' = sum numbers
  | otherwise = error "nah"
  where
    l = last chunks
    numbers = asNumber `concatMap` chunks

asNumber :: Chunk -> [Int]
asNumber (Int i) = [i]
asNumber _ = []

sumPart2 :: Day -> Int
sumPart2 [] = 0
sumPart2 ([Int x, Char o] : xs) = operator ints + sumPart2 (tailSafe rest)
  where
    -- lines are delimitered by an empty array
    (things, rest) = break (== []) ([Int x] : xs)
    -- only integers should be considered in this line
    ints = concatMap (concatMap asNumber) things
    operator
      | o == '*' = product
      | otherwise = sum
sumPart2 x = error ("nah " ++ show x)

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (_ : xs) = xs

-- Part 1
part1 :: (Day, Day) -> Int
part1 (day, _) = sum $ calculate <$> day

-- Part 2
part2 :: (Day, Day) -> Int
part2 (_, st) = sumPart2 st
