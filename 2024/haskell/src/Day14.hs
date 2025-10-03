module Day14 (parseDay, part1, part2) where

import Control.Applicative (liftA2)
import Data.Set (Set, fromList, member, toAscList)
import Debug.Trace (trace)
import NanoParsec (Parser, anyChar, number, parseTimes, runParser, spaces, star, string)
import Utils (traceInput)

type Coord = (Int, Int)

data Robot = Robot
  { pos :: Coord,
    vel :: Coord
  }
  deriving (Show)

parseSingleDay :: Parser Robot
parseSingleDay = do
  p <- coords "p="
  v <- coords "v="
  return $ Robot p v
  where
    coords st = string st *> coord <* spaces
    coord = liftA2 (,) number (anyChar *> number)

parseDay :: String -> [Robot] -- adjust type per puzzle
parseDay = runParser (star parseSingleDay)

simulate :: Coord -> [Robot] -> [Robot]
simulate (w, h) = map s'
  where
    s' (Robot (x, y) (dx, dy)) = Robot (mod' w (x + dx), mod' h (y + dy)) (dx, dy)

mod' :: Int -> Int -> Int
mod' m x
  | x < 0 = mod' m $ m + x
  | otherwise = x `mod` m

nTimes :: Int -> (b -> b) -> b -> b
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

addBotToScore :: Coord -> (Int, Int, Int, Int) -> Robot -> (Int, Int, Int, Int)
addBotToScore (w, h) (p1, p2, p3, p4) (Robot (x, y) _)
  | x < w2 && y < h2 = (p1 + 1, p2, p3, p4)
  | x > w2 && y < h2 = (p1, p2 + 1, p3, p4)
  | x < w2 && y > h2 = (p1, p2, p3 + 1, p4)
  | x > w2 && y > h2 = (p1, p2, p3, p4 + 1)
  | otherwise = (p1, p2, p3, p4)
  where
    w2 = w `div` 2
    h2 = h `div` 2

printBots :: Coord -> Int -> [Robot] -> String
printBots (w, h) c bots
  | isTree locs = unlines (("----------------------------- " ++ show c) : strings)
  | otherwise = printBots (w, h) (c + 1) (simulate (w, h) bots)
  where
    locs = fromList $ map pos bots
    strings = map line [0 .. h]
    line y = map (\x -> ((x, y) `member` locs) ? ('█' :? '.')) [0 .. w]
    w2 = w `div` 2

isTree :: Set Coord -> Bool
isTree s = hasJoint 8 0 $ toAscList s

hasJoint :: Int -> Int -> [Coord] -> Bool
hasJoint target current ((x1, y1) : (x2, y2) : xs)
  | target == current = True
  | x1 == x2 && y1 + 1 == y2 = hasJoint target (current + 1) $ (x2, y2) : xs
  | otherwise = hasJoint target 0 $ (x2, y2) : xs
hasJoint _ _ _ = False

data Cond a = a :? a

infixl 0 ?

infixl 1 :?

(?) :: Bool -> Cond a -> a
True ? (x :? _) = x
False ? (_ :? y) = y

tests = (11, 7)

real = (101, 103)

this = real

-- Part 2
part1 bots = p1 * p2 * p3 * p4
  where
    locs = nTimes 100 (simulate this) bots
    (p1, p2, p3, p4) = foldl (addBotToScore this) (0, 0, 0, 0) locs

-- Part 2
part2 bots = trace (printBots real 0 bots) ()
