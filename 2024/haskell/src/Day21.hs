{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day21 (parseDay, part1, part2) where

import Control.Lens hiding (element)
import Control.Monad.State
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP (between)
import Utils ((?:))

data AtNum = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9 | A deriving (Show, Eq, Ord)

atNumNeigh :: AtNum -> [(AtNum, AtPoint)]
atNumNeigh A1 = [(A2, R), (A4, U)]
atNumNeigh A4 = [(A1, D), (A7, U), (A5, R)]
atNumNeigh A7 = [(A4, D), (A8, R)]
atNumNeigh A0 = [(A2, U), (A, R)]
atNumNeigh A2 = [(A0, D), (A5, U), (A1, L), (A3, R)]
atNumNeigh A5 = [(A2, D), (A8, U), (A4, L), (A6, R)]
atNumNeigh A8 = [(A5, D), (A7, L), (A9, R)]
atNumNeigh A = [(A3, U), (A0, L)]
atNumNeigh A3 = [(A, D), (A6, U), (A2, L)]
atNumNeigh A6 = [(A3, D), (A9, U), (A5, L)]
atNumNeigh A9 = [(A6, D), (A8, L)]

data AtPoint = U | L | R | D | P deriving (Show, Eq, Ord)

atPointNeigh :: AtPoint -> [(AtPoint, AtPoint)]
atPointNeigh L = [(D, R)]
atPointNeigh D = [(L, L), (U, U), (R, R)]
atPointNeigh R = [(D, L), (P, U)]
atPointNeigh U = [(D, D), (P, R)]
atPointNeigh P = [(R, D), (U, L)]

data Move = MUp | MLeft | MDown | MRight deriving (Show, Eq, Ord)

type Coord = (Int, Int)

data St = St
  { _bot1 :: Map (AtNum, [AtNum]) [[AtPoint]],
    _bot2 :: Map (AtPoint, [AtPoint]) [[AtPoint]],
    _bot3 :: Map (AtPoint, [AtPoint]) [[AtPoint]]
  }
  deriving (Show)

makeLensesFor [("_bot1", "bot1"), ("_bot2", "bot2"), ("_bot3", "bot3")] ''St

parseDay :: String -> Int
parseDay st = 0

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (pure [])
  where
    f x xs = do x <- op x; if null x then xs else do { xs <- xs; pure $ x ++ xs }

tryIt :: (Applicative m) => m a -> Maybe a -> m a
tryIt _ (Just x) = pure x
tryIt o _ = o

shortestNum :: AtNum -> [AtNum] -> State St [[AtPoint]]
shortestNum _ [] = pure [[]]
shortestNum at (x : xs) = do
  cached <- (!? (at, x : xs)) <$> use bot1
  tryIt other cached
  where
    other = do
      el <- findNext empty at
      bot1 %= Map.insert (at, x : xs) el
      return el
    findNext :: Set AtNum -> AtNum -> State St [[AtPoint]]
    findNext done at'
      | at' == x = ((P :) <$>) <$> shortestNum at' xs
      | at' `member` done = pure []
      | otherwise = (\(newAt, don) -> ((don :) <$>) <$> findNext (at' `insert` done) newAt) `concatMapM` atNumNeigh at'

shortestMove :: Lens' St (Map (AtPoint, [AtPoint]) [[AtPoint]]) -> AtPoint -> [AtPoint] -> State St [[AtPoint]]
shortestMove _ _ [] = pure [[]]
shortestMove l at (x : xs) = do
  cached <- (!? (at, x : xs)) <$> use l
  tryIt other cached
  where
    other = do
      el <- findNext empty at
      l %= Map.insert (at, x : xs) el
      return el
    findNext :: Set AtPoint -> AtPoint -> State St [[AtPoint]]
    findNext done at'
      | at' == x = ((P :) <$>) <$> shortestMove l at' xs
      | at' `member` done = pure []
      | otherwise = (\(newAt, don) -> ((don :) <$>) <$> findNext (at' `insert` done) newAt) `concatMapM` atPointNeigh at'

moveIt :: Move -> Coord -> Coord
moveIt MUp (x, y) = (x, y - 1)
moveIt MDown (x, y) = (x, y + 1)
moveIt MLeft (x, y) = (x - 1, y)
moveIt MRight (x, y) = (x + 1, y)

dist :: Coord -> Coord -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Part 2
part1 _ = filter ((== minL) . length) options
  where
    minL = minimum (length <$> options)
    options = evalState (shortestNum A [A0, A2, A9, A]) (St Map.empty Map.empty Map.empty)

filterIt :: Int -> [[a]] -> [[a]]
filterIt lay xs = ((< m) . length) `filter` xs
  where
    m = minimum (length <$> xs) + lay

-- Part 2
part2 _ = (o, minL)
  where
    o = filter ((== minL) . length) options
    minL = minimum (length <$> options)
    b1 = shortestNum A [A0, A2, A9, A]
    b2 = shortestMove bot2 P
    b3 = shortestMove bot3 P
    optionMonad = b1 >>= concatMapM b2 . filterIt 1 >>= concatMapM b3 . filterIt 1
    options = evalState optionMonad (St Map.empty Map.empty Map.empty)
