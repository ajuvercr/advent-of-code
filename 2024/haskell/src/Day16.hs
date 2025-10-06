{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day16 (parseDay, part1, part2) where

import Control.Lens hiding (element)
import Control.Monad.State (State, evalState, runState)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Set (Set, deleteMin, empty, fromList, insert, lookupMin, member, size, toAscList, union)

data Move = MUp | MLeft | MDown | MRight deriving (Show, Eq, Ord)

type Coord = (Int, Int)

type CoordDir = (Coord, Move)

data St = St
  { _done :: Map CoordDir Int,
    _open :: Set Coord,
    _todo :: Set (Int, CoordDir, [CoordDir]),
    _seen :: Map CoordDir [[CoordDir]]
  }
  deriving (Show)

makeLensesFor [("_done", "done"), ("_open", "open"), ("_todo", "todo"), ("_seen", "seen")] ''St

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

clw :: Move -> Move
clw MUp = MLeft
clw MLeft = MDown
clw MDown = MRight
clw MRight = MUp

cclw :: Move -> Move
cclw MUp = MRight
cclw MRight = MDown
cclw MDown = MLeft
cclw MLeft = MUp

getMin :: State St (Int, CoordDir, [CoordDir])
getMin = do
  m <- lookupMin <$> use todo
  case m of
    Just o -> todo %= deleteMin >> pure o
    Nothing -> error "something went wrong"

findMoves :: Set Coord -> (Int, CoordDir, [CoordDir]) -> [(Int, CoordDir, [CoordDir])]
findMoves o (p, (l, m), f) = asOutput `Prelude.map` (allowed `filter` options)
  where
    allowed (m', _) = moveIt m' l `member` o
    options = [(m, p + 1), (clw m, p + 1000), (cclw m, p + 1000)]
    -- If the direction didn't change, we moving
    asOutput (m', p')
      | m == m' = (p', (moveIt m' l, m'), (moveIt m' l, m') : f)
      | otherwise = (p', (l, m'), (l, m') : f)

moveOne :: Coord -> State St (Int, [CoordDir])
moveOne target = getMin >>= moveOne' target

-- checks if the current evaluting location is already in done
-- If not, add it and continue
moveOne' :: Coord -> (Int, CoordDir, [CoordDir]) -> State St (Int, [CoordDir])
moveOne' target (score, (t', dir), f)
  | target == t' = pure (score, f)
  | otherwise = do
      d <- use done
      let m = d !? (t', dir)
      case m of
        Nothing -> done %= Map.insert (t', dir) score >> moveOne'' target (score, (t', dir), f)
        Just x
          | x == score -> seen %= Map.insertWith (++) (t', dir) [f] >> moveOne target
          | otherwise -> moveOne target

-- Find all moves, and add them to todo
moveOne'' :: Coord -> (Int, CoordDir, [CoordDir]) -> State St (Int, [CoordDir])
moveOne'' target current = do
  o <- use open
  let ts = fromList $ findMoves o current
  todo %= union ts
  moveOne target

-- Part 2
-- too high 106516
part1 :: (Set Coord, Coord, Coord) -> Int
part1 (o, start, end) = x
  where
    (x, _) = evalState (moveOne end) (St Map.empty o td Map.empty)
    td = fromList [(0, (start, MRight), [])]

findRoutes :: Map CoordDir [[CoordDir]] -> Set CoordDir -> [CoordDir] -> Set CoordDir
findRoutes _ found [] = found
findRoutes look f (x : xs) = next (x `member` f) (look !? x)
  where
    next False (Just other) = findRoutes look (foldl (findRoutes look) added other) xs
    next _ _ = findRoutes look added xs
    added = x `insert` f

-- Part 2
part2 :: (Set Coord, Coord, Coord) -> Int
part2 (o, start, end) = size routes
  where
    routes = fromList $ Prelude.map fst $ toAscList allRoutes
    allRoutes = findRoutes s empty xs
    ((_, xs), St _ _ _ s) = runState (moveOne end) (St Map.empty o td Map.empty)
    td = fromList [(0, (start, MRight), [(start, MRight)])]
