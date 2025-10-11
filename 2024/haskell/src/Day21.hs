{-# LANGUAGE RankNTypes #-}

module Day21 (parseDay, part1, part2) where

import Control.Monad.State
import Data.Functor (($>))
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Set (empty, insert, member)

data AtNum = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9 | A deriving (Show, Eq, Ord)

fromChar :: Char -> AtNum
fromChar '0' = A0
fromChar '1' = A1
fromChar '2' = A2
fromChar '3' = A3
fromChar '4' = A4
fromChar '5' = A5
fromChar '6' = A6
fromChar '7' = A7
fromChar '8' = A8
fromChar '9' = A9
fromChar 'A' = A
fromChar _ = error "invalid char"

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

findPahts :: (Ord a) => (a -> [(a, AtPoint)]) -> a -> a -> [[AtPoint]]
findPahts f start end = (P :) <$> findPahts' empty start
  where
    findPahts' done c
      | c == end = [[P]]
      | c `member` done = []
      | otherwise = (\(newAt, c') -> (c' :) <$> findPahts' (c `insert` done) newAt) `concatMap` f c

pathPairs :: [a] -> [(a, a)]
pathPairs [] = []
pathPairs [_] = []
pathPairs (x : y : xs) = (x, y) : pathPairs (y : xs)

data Move = MUp | MLeft | MDown | MRight deriving (Show, Eq, Ord)

parseDay :: String -> [(Int, [AtNum])]
parseDay st = readOne <$> (/= "") `filter` lines st

chopOne :: [a] -> [a]
chopOne [] = []
chopOne [_] = []
chopOne (x : xs) = x : chopOne xs

readOne :: String -> (Int, [AtNum])
readOne st = (read $ chopOne st, r' st)
  where
    r' [] = []
    r' (x : xs) = fromChar x : r' xs

-- Part 2
part1 :: [(Int, [AtNum])] -> Int
part1 x = o
  where
    o = evalState things Map.empty
    things = sum <$> getOne `mapM` x
    getOne (c, xs) = (* c) <$> findShortest 1 xs

type P2 = State (Map (Int, (AtPoint, AtPoint)) Int) Int

findShortest :: Int -> [AtNum] -> P2
findShortest i x = sum <$> things
  where
    things = findThing `mapM` allPaths
    findThing p = minimum <$> (resolvePath i `mapM` p)
    path = pathPairs (A : x)
    allPaths = uncurry (findPahts atNumNeigh) <$> path

resolvePath :: Int -> [AtPoint] -> P2
resolvePath i path = options
  where
    ps = pathPairs path
    options = sum <$> shortestPath i `mapM` ps

shortestPath :: Int -> (AtPoint, AtPoint) -> P2
shortestPath i (s, e) = do
  cached <- gets (!? (i, (s, e)))
  case cached of
    Just x -> pure x
    Nothing -> apply `mapM` paths >>= ins . minimum
  where
    ins :: Int -> P2
    ins x = modify (Map.insert (i, (s, e)) x) $> x
    paths = findPahts atPointNeigh s e
    apply = if i == 0 then (\x -> pure $ length x - 1) else resolvePath (i - 1)

-- Part 2
part2 :: [(Int, [AtNum])] -> Int
part2 x = o
  where
    o = evalState things Map.empty
    things = sum <$> getOne `mapM` x
    getOne (c, xs) = (* c) <$> findShortest 24 xs
