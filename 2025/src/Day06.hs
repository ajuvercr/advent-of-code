module Day06 (parseDay, part1, part2) where

import Data.Maybe (isNothing)
import Data.Set (Set, empty, insert, member)

data Move = MUp | MLeft | MDown | MRight deriving (Show, Eq, Ord)

type Coord = (Int, Int)

type CoordDir = (Coord, Move)

clw :: Move -> Move
clw MUp = MRight
clw MRight = MDown
clw MDown = MLeft
clw MLeft = MUp

--  Parsing
parseDay :: String -> (Set Coord, Coord, Coord)
parseDay st = p st (0, 0)
  where
    p :: String -> Coord -> (Set Coord, Coord, Coord)
    p [] _ = (empty, (0, 0), (0, 0))
    p ('\n' : xs) (_, j) = p xs (0, j + 1)
    p (x : xs) (i, j)
      | x == '#' = (added, s, e')
      | x == '^' = (o, (i, j), e')
      | otherwise = (o, s, e')
      where
        e' = (max i i', max j j')
        (o, s, (i', j')) = p xs (i + 1, j)
        added = (i, j) `insert` o

moveIt :: Move -> Coord -> Coord
moveIt MUp (x, y) = (x, y - 1)
moveIt MDown (x, y) = (x, y + 1)
moveIt MLeft (x, y) = (x - 1, y)
moveIt MRight (x, y) = (x + 1, y)

step :: Coord -> Set Coord -> CoordDir -> Set CoordDir -> Maybe (Set Coord)
step (mx, my) obs ((x, y), dir) done
  | ((x, y), dir) `member` done = Nothing
  | x > mx || y > my || x < 0 || y < 0 = Just empty
  | st `member` obs = step (mx, my) obs ((x, y), clw dir) done'
  | otherwise = ((x, y) `insert`) <$> step (mx, my) obs (st, dir) done'
  where
    st = moveIt dir (x, y)
    done' = ((x, y), dir) `insert` done

-- Part 1
part1 :: (Set Coord, Coord, Coord) -> Maybe Int
part1 (a, b, c) = length <$> step c a (b, MUp) empty

isInfinite' :: Set Coord -> (Set Coord, Coord, Coord) -> Coord -> Bool
isInfinite' ori (a, b, c) i
  | i `member` a = False
  | i `member` ori = isNothing $ step c (i `insert` a) (b, MUp) empty
  | otherwise = False

-- Part 2
part2 :: (Set Coord, Coord, (Int, Int)) -> Int
part2 (a, b, c) = length $ fil `filter` tries
  where
    ori = step c a (b, MUp) empty
    f (Just x) = x
    f Nothing = empty
    tries = [(i, j) | i <- [0 .. fst c], j <- [0 .. snd c]]
    fil = isInfinite' (f ori) (a, b, c)
