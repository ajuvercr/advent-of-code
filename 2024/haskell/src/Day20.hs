{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Day20 (parseDay, part1, part2) where

import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Utils ((?:))

data Move = MUp | MLeft | MDown | MRight deriving (Show, Eq, Ord)

type Coord = (Int, Int)

data St = St
  { _done :: Set Coord,
    _open :: Set Coord,
    _todo :: Set (Int, Coord)
  }
  deriving (Show)

parseDay :: String -> (Set Coord, Coord, Coord)
parseDay st = p st (0, 0)
  where
    p :: String -> Coord -> (Set Coord, Coord, Coord)
    p [] _ = (empty, (0, 0), (0, 0))
    p ('\n' : xs) (_, j) = p xs (0, j + 1)
    p (x : xs) (i, j)
      | x == '.' = (added, s, e)
      | x == 'S' = (added, (i, j), e)
      | x == 'E' = (added, s, (i, j))
      | otherwise = (o, s, e)
      where
        (o, s, e) = p xs (i + 1, j)
        added = (i, j) `insert` o

moveIt :: Move -> Coord -> Coord
moveIt MUp (x, y) = (x, y - 1)
moveIt MDown (x, y) = (x, y + 1)
moveIt MLeft (x, y) = (x - 1, y)
moveIt MRight (x, y) = (x + 1, y)

dist :: Coord -> Coord -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

toMap' :: Int -> [Coord] -> Set Coord -> Map Coord Int -> Map Coord Int
toMap' at' todo' coords done'
  | null t'' = m'
  | otherwise = toMap' (at' + 1) t'' coords m'
  where
    t'' = (\x -> not $ x `Map.member` m') `filter` t'
    (m', t') = foldl addx (done', []) todo'
    addx :: (Map Coord Int, [Coord]) -> Coord -> (Map Coord Int, [Coord])
    addx (m, t) x
      | Map.member x m = (m, t) -- already done
      | otherwise = (Map.insert x at' m, t ++ neighs x coords) -- add it and their neighbours

neighs :: Coord -> Set Coord -> [Coord]
neighs x coords = (`member` coords) `filter` ((`moveIt` x) <$> [MUp, MRight, MLeft, MDown])

-- -- let dist be a |V| × |V| array of minimum distances initialized to ∞ (infinity)
-- -- for each edge (u, v) do
-- --     dist[u][v] = w(u, v)  // The weight of the edge (u, v)
-- -- for each vertex v do
-- --     dist[v][v] = 0
-- -- for k from 1 to |V|
-- --     for i from 1 to |V|
-- --         for j from 1 to |V|
-- --             if dist[i][j] > dist[i][k] + dist[k][j]
-- --                 dist[i][j] = dist[i][k] + dist[k][j]
-- --             end if
-- warshall :: Set Coord -> Map (Coord, Coord) Int
-- warshall coords = foldr warshall' start (trace ("tc " ++ show (length ts)) ts)
--   where
--     ts = triples coordL
--     coordL = Set.toAscList coords
--     warshall' :: (Coord, Coord, Coord) -> Map (Coord, Coord) Int -> Map (Coord, Coord) Int
--     warshall' (k, i, j) m
--       | get' m i j > get' m i k + get' m k j = Map.insert (i, j) (get' m i k + get' m k j) m
--       | otherwise = m
--     get' :: Map (Coord, Coord) Int -> Coord -> Coord -> Int
--     get' m a b = (m !? (a, b)) ?: 1000000000
--     start = Map.fromList $ selves ++ neighs'
--     selves = (\x -> ((x, x), 0)) <$> coordL
--     neighsOf x = concatMap (\t -> ([((x, moveIt t x), 1) | moveIt t x `member` coords])) [MUp, MRight, MLeft, MDown]
--     neighs' = concatMap neighsOf coordL
--
-- triples :: [a] -> [(a, a, a)]
-- triples xs = concatMap a' (trace ("xs " ++ show (length xs)) xs)
--   where
--     a' a = concatMap (b' a) xs
--     b' a b = c' a b <$> xs
--     c' a b c = (a, b, c)

cheatsFor :: Int -> Coord -> [(Coord, Coord)]
cheatsFor m' (i, j) = ((i, j),) <$> options
  where
    options =
      [ (i + dx, j + dy)
        | dx <- [-m' .. m'],
          dy <- [-m' .. m'],
          dist (i, j) (i + dx, j + dy) <= m'
      ]

countCheats :: Int -> Int -> Map Coord Int -> Map Coord Int -> [Coord] -> Int
countCheats t c start end todo' = length $ isGoodCheat `filter` things
  where
    things = concatMap (cheatsFor c) todo'
    isGoodCheat :: (Coord, Coord) -> Bool
    isGoodCheat (s, e) = t >= ((start !? e) ?: 10000) + ((end !? s) ?: 10000) + dist s e

-- Part 2
part1 :: (Set Coord, Coord, Coord) -> Int
part1 (o, start, end) = countCheats d 2 startL endL (Set.toAscList o)
  where
    d = endL ! start - 100
    endL = toMap' 0 [end] o Map.empty
    startL = toMap' 0 [start] o Map.empty

-- Part 2
part2 :: (Set Coord, Coord, Coord) -> Int
part2 (o, start, end) = countCheats d 20 startL endL (Set.toAscList o)
  where
    d = endL ! start - 100
    endL = toMap' 0 [end] o Map.empty
    startL = toMap' 0 [start] o Map.empty
