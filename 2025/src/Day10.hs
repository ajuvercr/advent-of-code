{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Day10 (parseDay, part1, part2) where

import Control.Monad (filterM)
import qualified Control.Monad.State as St
import Data.Foldable (foldlM)
import Data.Functor (($>))
import Data.List (sortBy, transpose)
import qualified Data.Matrix as Matrix
import Data.Matrix.SmithNormalForm (smithNormalForm)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import GHC.Base ((<|>))
import NanoParsec (char, delimitered, number, oneOf, runParser, spaces', star)
import Utils (adjust, traceOutput)

type State = [Bool]

type Buttons = [[Int]]

type Day = [(State, Buttons, [Int])]

parseDay :: String -> Day -- adjust type per puzzle
parseDay inp = runParser parseMachine <$> lines inp
  where
    parseTarget = char '[' *> star ((== '#') <$> oneOf ".#") <* char ']'
    parseButton = char '(' *> delimitered "," number <* char ')'
    parseButtons = star $ spaces' *> parseButton <* spaces'
    parseVoltage = char '{' *> delimitered "," number <* char '}'
    parseMachine = (,,) <$> parseTarget <*> parseButtons <*> parseVoltage

push' :: State -> [Int] -> State
push' x y = push y x

push :: [Int] -> State -> State
push [] x = x
push (x : xs) s = push xs s'
  where
    s' = adjust not x s

pushVoltage :: [Int] -> [Int] -> [Int]
pushVoltage at [] = at
pushVoltage at (x : xs) = pushVoltage at' xs
  where
    at' = adjust (+ 1) x at

unPushVoltage :: [Int] -> [Int] -> [Int]
unPushVoltage at [] = at
unPushVoltage at (x : xs) = pushVoltage at' xs
  where
    at' = adjust (+ (-1)) x at

filterVoltage :: [Int] -> [Int] -> St.State (Set [Int]) Bool
filterVoltage a b
  | tooBig = pure False
  | otherwise = do
      s <- St.get
      (if b `Set.member` s then pure False else St.put (b `Set.insert` s) $> True)
  where
    tooBig = any (uncurry (<)) (zip a b)

pushButtons :: (Ord a, Monad m) => (a -> m Bool) -> (a -> [Int] -> a) -> Buttons -> Set a -> m (Set a)
pushButtons fi p buttons start = foldlM (\x y -> Set.union x <$> from y) Set.empty (Set.toList start)
  where
    from s = Set.fromList <$> fi `filterM` (p s <$> buttons)

untilFinishes :: (Ord a, Monad m) => (a -> m Bool) -> (a -> [Int] -> a) -> a -> Buttons -> Set a -> m Int
untilFinishes fi p target bs s
  | target `Set.member` s = pure 0
  | otherwise = do
      ns <- pushButtons fi p bs s
      c <- untilFinishes fi p target bs ns
      return $ 1 + c

untilFinishes' :: (State, Buttons, [Int]) -> Maybe Int
untilFinishes' (a, b, _) = untilFinishes (const $ Just True) push' a b start
  where
    start = Set.singleton (replicate (length a) False)

untilFinishesPart2 :: (State, Buttons, [Int]) -> St.State (Set [Int]) Int
untilFinishesPart2 (_, b, a) = untilFinishes (filterVoltage a) pushVoltage a b start
  where
    start = Set.singleton (replicate (length a) 0)

-- Part 2
-- part1 :: Day -> Maybe Int
part1 day = untilFinishes' <$> day

-- Part 2
-- part2 day = sum $ once <$> day
--   where
--     once x = St.evalState (untilFinishesPart2 x) Set.empty

sp :: [Int] -> [Int] -> Maybe Int -> Int -> Bool
sp _ _ Nothing _ = False
sp target at (Just b) i = i + maxDif > b
  where
    maxDif = maximum $ dif <$> zip target at
    dif (a, b) = abs (a - b)

dfs :: [Int] -> Buttons -> [Int] -> Maybe Int -> Int -> Maybe Int
dfs _ [] _ x _ = x
dfs target (b : bs) at bound i
  | shouldPrune = bound
  | bound == Just i = bound
  | target == at = trace "Found one" Just i
  | tooBig = bound
  | otherwise = do
      let applied = dfs target (b : bs) pushed bound (i + 1)
      dfs target bs at applied i
  where
    shouldPrune = sp target at bound i
    pushed = pushVoltage at b
    tooBig = any (uncurry (<)) (zip target at)

dfs' :: [Int] -> Buttons -> [Int] -> Maybe Int
dfs' _ [] _ = Nothing
dfs' target (b : bs) at
  | tooBig = Nothing
  | target == at = trace "Found one" Just 0
  | otherwise = (+ 1) <$> dfs' target (b : bs) pushed <|> dfs' target bs at
  where
    pushed = pushVoltage at b
    tooBig = any (uncurry (<)) (zip target at)

part2Dfs ((_, buttons, target) : _) = dfs' target (trace (show sorted) sorted) start
  where
    start = replicate (length target) 0
    buttonOrd :: [Int] -> Int
    buttonOrd bs = minimum $ (target !!) <$> bs
    sorted = sortBy (comparing buttonOrd) buttons

buildColumn :: Int -> Int -> [Int] -> [Integer]
buildColumn m a []
  | m <= a = []
  | otherwise = 0 : buildColumn m (a + 1) []
buildColumn m a (x : xs)
  | a == x = 1 : buildColumn m (a + 1) xs
  | otherwise = 0 : buildColumn m (a + 1) (x : xs)

-- part2 (_ : x : xs) = St.evalState (untilFinishesPart2 x) Set.empty
part2 ((_, buttons, target) : _) = (mat, normalForm)
  where
    mats@(f : _) = transpose $ buildColumn (length target) 0 <$> buttons
    mat = Matrix.fromList (length mats) (length f) (concat mats) :: Matrix.Matrix Integer
    normalForm = smithNormalForm mat

-- part2 _ = = do
--   result <- optimize Lexicographic $ do
--     x <- sInteger "x"
--     y <- sInteger "y"
--
--     constrain $ x + y .>= 10
--     constrain $ x - y .<= 3
--
--     minimize "objective" (x + y)
--
-- print result

-- part2 ((a, b, c) : _) = pushButtons (filterVoltage c) pushVoltage b start
--   where
--     start = Set.singleton (replicate (length c) 0)
