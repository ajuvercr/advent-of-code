module Day09 (parseDay, part1, part2) where

import Data.List (find, sort)
import Data.Set (fromList, toList)
import Debug.Trace (trace)
import GHC.Base (Alternative (some))
import Utils (orElse, split)

type Coord = (Int, Int)

type Line = (Coord, Coord)

type Day = [Coord]

--  Parsing
parseDay :: String -> Day -- adjust type per puzzle
parseDay x = toCoord <$> lines x
  where
    toCoord x' = toTuple $ read <$> split ',' x'
    toTuple [a, b] = (a, b)
    toTuple _ = error "not a tuple"

-- Part 1
part1 :: Day -> Int
part1 s = maximum $ tryOne <$> s
  where
    tryOne x = maximum $ opp x <$> s

opp :: Coord -> Coord -> Int
opp (a, b) (c, d) = dx * dy
  where
    dx = 1 + abs (a - c)
    dy = 1 + abs (b - d)

countIntersections :: [Line] -> Coord -> Int -> Int
countIntersections [] _ x = x
countIntersections (((at, y1), (_, y2)) : xs) (a, b) i
  | at > a = i
  | otherwise = countIntersections xs (a, b) (i + c)
  where
    c
      | b >= y1 && b < y2 = 1
      | otherwise = 0

inside :: [Line] -> Coord -> Bool
inside a b = odd $ countIntersections a b 0

bump :: Coord -> Coord
bump (a, b) = (a + 1, b)

testPoints :: [Coord] -> [Coord]
testPoints cs = [(x, y) | x <- xs, y <- ys]
  where
    xs = toList $ fromList $ fst <$> cs
    ys = toList $ fromList $ snd <$> cs

type Square = Line

validSquare :: [Coord] -> Square -> Bool
validSquare cs ((a, b), (c, d)) = not $ any isInside cs
  where
    isInside (x, y) = x >= a && x <= c && y >= b && y <= d

onLine :: Coord -> (Coord, Coord) -> Bool
onLine (x, y) ((a, b), (c, d)) = y <= ma_y && y >= mi_y && x <= ma_x && x >= mi_x
  where
    mi_x = min a c
    ma_x = max a c
    mi_y = min b d
    ma_y = max b d

size :: Square -> Int
size ((a, b), (c, d)) = (c - a + 1) * (d - b + 1)

-- Part 2
-- too low 2202158
part2 points@(x : xs) = maximum $ size <$> squares
  where
    -- part2 points@(x : xs) = countIntersections vertLines (9, 3) 0

    squares = validSquare tp `filter` [((min a c, min b d), (max a c, max b d)) | x@(a, b) <- points, y@(c, d) <- points, x > y]
    tp = checkPoint `filter` testPoints points
    checkPoint x = not (any (onLine x) allLines || inside vertLines x)
    allLines = zip points (xs ++ [x])
    vertLines = sort $ sortLine <$> isVertical `filter` zip points (xs ++ [x])
    horLines = isHorizontal `filter` zip points (xs ++ [x])
    isVertical ((a, _), (b, _)) = a == b
    isHorizontal ((_, a), (_, b)) = a == b
    sortLine (a, b)
      | a == min a b = (a, b)
      | otherwise = (b, a)
part2 _ = error "nah"
