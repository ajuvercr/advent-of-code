module Day12 (parseDay, part1, part2) where

import Control.Monad.State
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Map (Map, insert, (!?))
import GHC.Base (quotRemInt)
import NanoParsec (digit, plus, runParser, spaces, star)

type Day = [[Int]]

num :: Char -> Int
num '0' = 0
num '1' = 1
num '2' = 2
num '3' = 3
num '4' = 4
num '5' = 5
num '6' = 6
num '7' = 7
num '8' = 8
num '9' = 9
num x = read [x]

parseDay :: String -> Day -- adjust type per puzzle
parseDay = runParser parser
  where
    parser = star (plus (digit <&> num) <* spaces)

-- | Split a list.
splitHalf :: [a] -> ([a], [a])
splitHalf xs = go xs xs
  where
    go (y : ys) (_ : _ : zs) = first (y :) (go ys zs)
    go ys _ = ([], ys)

toNum :: [Int] -> Int
toNum = foldl (\x -> (+) (10 * x)) 0

toArr :: Int -> [Int]
toArr 0 = []
toArr x = toArr i ++ [j]
  where
    (i, j) = quotRemInt x 10

timesIt :: [Int] -> [Int]
timesIt x = toArr $ 2024 * toNum x

pruneZero :: [Int] -> [Int]
pruneZero [0] = [0]
pruneZero (0 : xs) = pruneZero xs
pruneZero y = y

type MS a = State (Map (Int, [Int]) Int) a

getI :: Int -> [Int] -> MS (Maybe Int)
getI x y = do
  st <- get
  let o = st !? (x, y)
  return o

putI :: Int -> [Int] -> Int -> MS Int
putI x y o = do
  st <- get
  put $ insert (x, y) o st
  return o

countBlinkLength :: Int -> [Int] -> MS Int
countBlinkLength 0 _ = pure 1
countBlinkLength i x = do
  m <- getI i x
  case m of
    Just out -> return out
    _ -> count >>= putI i x
  where
    nextBlink = countBlinkLength (i - 1)
    nextBlinks = mapM nextBlink
    step = nextBlinks . blink
    count = sum <$> step x

blink :: [Int] -> [[Int]]
blink [0] = [[1]]
blink x | even (length x) = [pruneZero i, pruneZero j]
  where
    (i, j) = splitHalf x
blink x = [timesIt x]

-- Part 2
part1 :: Day -> Int
part1 day = sum o
  where
    o = evalState (mapM (countBlinkLength 25) day) mempty

-- Part 2
part2 :: Day -> Int
part2 day = sum o
  where
    o = evalState (mapM (countBlinkLength 75) day) mempty
