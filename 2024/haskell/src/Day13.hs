module Day13 (parseDay, part1, part2) where

import Control.Applicative (liftA2)
import NanoParsec (Parser, anyChar, number, parseTimes, runParser, spaces, star, string)

type Coord = (Int, Int)

data Day = Day
  { buttonA :: Coord,
    buttonB :: Coord,
    target :: Coord
  }
  deriving (Show)

parseSingleDay :: Parser Day
parseSingleDay = do
  ba <- coords "Button A: "
  bb <- coords "Button B: "
  t <- coords "Prize: "
  return $ Day ba bb t
  where
    coords st = string st *> coord <* spaces
    coord = liftA2 (,) (parseTimes 2 anyChar *> number) (parseTimes 4 anyChar *> number)

parseDay :: String -> [Day] -- adjust type per puzzle
parseDay = runParser (star parseSingleDay)

findSolutions :: Day -> Int
findSolutions (Day (a1, a2) (b1, b2) (t1, t2)) = s
  where
    numerator = a2 * t1 - a1 * t2
    denominator = a2 * b1 - a1 * b2
    (y, r1) = numerator `divMod` denominator
    (x, r2) = (t2 - y * b2) `divMod` a2
    s
      | r1 == 0 && r2 == 0 = x * 3 + y
      | otherwise = 0

toPart2 :: Day -> Day
toPart2 (Day a b (x, y)) = Day a b (x + 10000000000000, y + 10000000000000)

-- Part 2
part1 :: [Day] -> Int
part1 days = sum $ map findSolutions days

-- Part 2
part2 :: [Day] -> Int
part2 days = sum $ map (findSolutions . toPart2) days
