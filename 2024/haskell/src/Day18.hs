{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Day18 (parseDay, part1, part2) where

import Data.Set (Set, deleteMin, empty, fromList, insert, lookupMin, member, union)
import NanoParsec (char, number, runParser, spaces, star)

data Move = MUp | MLeft | MDown | MRight deriving (Show, Eq, Ord)

type Coord = (Int, Int)

type Day = [Coord]

moveIt :: Move -> Coord -> Coord
moveIt MUp (x, y) = (x, y - 1)
moveIt MDown (x, y) = (x, y + 1)
moveIt MLeft (x, y) = (x - 1, y)
moveIt MRight (x, y) = (x + 1, y)

parseDay :: String -> Day
parseDay = runParser $ star ((,) <$> number <* char ',' <*> number <* spaces)

real :: ((Int, Int), Int)
real = ((70, 70), 1024)

this :: (Int, Int)
takeAmount :: Int
(this, takeAmount) = real

mx :: Int
my :: Int
(mx, my) = this

inBounds :: Coord -> Bool
inBounds (i, j) = i >= 0 && j >= 0 && i <= mx && j <= my

allowed :: Set Coord -> Coord -> Bool
allowed set m = not $ m `member` set

find :: Set Coord -> Set Coord -> Set (Int, [Coord], Coord) -> Maybe (Int, [Coord])
find coords done todo = lookupMin todo >>= find'
  where
    find' (p, trail, at)
      | at `member` done = find coords done (deleteMin todo)
      | at == this = Just (p, trail)
      | otherwise = find coords (at `insert` done) todo'
      where
        newLocations = Prelude.map (p + 1,at : trail,) $ Prelude.filter (allowed coords) $ Prelude.filter inBounds $ Prelude.map (`moveIt` at) [MUp, MDown, MLeft, MRight]
        todo' = deleteMin todo `union` fromList newLocations

findWrong' :: Set Coord -> [Coord] -> Set Coord -> Coord
findWrong' _ [] _ = error "cannot happend"
findWrong' trail (x : xs) droped
  | x `member` trail = findWrong (x : xs) droped
  | otherwise = findWrong' trail xs (x `insert` droped)

findWrong :: [Coord] -> Set Coord -> Coord
findWrong (x : xs) added = case find added' empty (fromList [(0, [], (0, 0))]) of
  Just (_, trail) -> findWrong' (fromList trail) xs added'
  Nothing -> x
  where
    added' = x `insert` added
findWrong _ _ = (0, 0)

-- Part 2
-- too high 106516
part1 :: [Coord] -> Maybe Int
part1 bytes = fst <$> find droped empty (fromList [(0, [], (0, 0))])
  where
    droped = fromList $ take takeAmount bytes

part2 :: [Coord] -> Coord
part2 bytes = findWrong bytes empty
