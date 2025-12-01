module Day09 (parseDay, part1, part2) where

import Control.Monad.State
import Data.Char (isDigit)
import Data.Foldable (toList)

type Day = ([Maybe Int], [Part2])

type Part2 = Either Int (Int, Int)

--  Parsing
parseDay :: String -> Day -- adjust type per puzzle
parseDay x = (inp 0 red, inp2 0 red)
  where
    filtered = filter isDigit x
    red = map (read . (: [])) filtered

inp :: Int -> [Int] -> [Maybe Int]
inp cId (used : free : rest) = replicate used (Just cId) ++ replicate free Nothing ++ inp (cId + 1) rest
inp cId [used] = replicate used $ Just cId
inp _ [] = []

inp2 :: Int -> [Int] -> [Part2]
inp2 cId (used : free : rest) = [Right (cId, used), Left free] ++ inp2 (cId + 1) rest
inp2 cId [used] = [Right (cId, used)]
inp2 _ [] = []

step :: Int -> [Maybe Int] -> [Int] -> [Int]
step 0 _ _ = []
step i (Just x : xs) other = x : step (i - 1) xs other
step i (Nothing : xs) (o : os) = o : step (i - 1) xs os
step _ _ _ = []

collect :: (Foldable t) => [t a] -> [a]
collect = concatMap toList

-- Part 1
part1 :: Day -> Int
part1 (input, _) = checksum $ step (length reveresed) input reveresed
  where
    reveresed = reverse $ collect input

checksum :: [Int] -> Int
checksum = _check 0
  where
    _check _ [] = 0
    _check c (x : xs) = c * x + _check (c + 1) xs

findPart2 :: Int -> [(Int, Int)] -> Maybe ((Int, Int), [(Int, Int)])
findPart2 _ [] = Nothing
findPart2 f ((cId, l) : xs) | f >= l = Just ((cId, l), xs)
findPart2 f (r : xs) = do
  (found, other) <- findPart2 f xs
  return (found, r : other)

findPart :: Int -> State [(Int, Int)] (Maybe (Int, Int))
findPart count = do
  n <- get
  case findPart2 count n of
    Just (found, st) -> do
      put st
      return $ Just found
    Nothing -> return Nothing

filterOut :: (Eq a) => a -> Maybe a -> [a] -> [a]
filterOut a Nothing = filter (/= a)
filterOut a (Just to) = map mapper
  where
    mapper b | b == a = to
    mapper b = b

step2 :: Int -> [Part2] -> State [(Int, Int)] [(Int, Int)]
step2 0 _ = pure []
step2 _ [] = pure []
step2 c (Right x : xs) = modify (filterOut x Nothing) >> (x :) <$> step2 (c - 1) xs
step2 c (Left x : xs) = do
  y <- findPart x
  case y of
    Just (cId, le) -> ((cId, le) :) <$> step2 (c - 1) (filterOut (Right (cId, le)) (Just $ Left le) (Left (x - le) : xs))
    Nothing -> ((0, x) :) <$> step2 c xs

part2ToPart1 :: [(Int, Int)] -> [Int]
part2ToPart1 [] = []
part2ToPart1 ((cId, count) : xs) = replicate count cId ++ part2ToPart1 xs

-- Part 2
part2 :: Day -> Int
part2 (_, input) = checksum $ part2ToPart1 o
  where
    o = evalState (step2 (length input) input) reveresed
    reveresed = reverse $ collect input
