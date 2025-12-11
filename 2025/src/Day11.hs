{-# LANGUAGE TupleSections #-}

module Day11 (parseDay, part1, part2) where

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Utils (split)

type Day = [(String, String)]

type Counts = Map String (Int, Int, Int)

parseLine :: String -> [(String, String)]
parseLine x = (first,) <$> split ' ' sn
  where
    (first, s2) = break (== ':') x
    sn = tail $ tail s2

parseDay :: String -> Day -- adjust type per puzzle
parseDay x = parseLine `concatMap` lines x

chooseNext :: Day -> Maybe String
chooseNext xs = find f $ snd <$> xs
  where
    f x = all ((/= x) . fst) xs

insertPath :: Day -> String -> Counts -> Counts
insertPath day target counts = foldl updateOne counts starts
  where
    (a, b, c) = counts Map.! target
    starts = fst <$> ((== target) . snd) `filter` day
    updateOne cs t = Map.update (Just . u) t cs
    u (a1, b2, c2)
      | target == "dac" || target == "fft" = (a1 + a, b2 + a, c2 + b)
      | otherwise = (a1 + a, b2 + b, c2 + c)

step :: (Day, Counts) -> Maybe String -> (Day, Counts)
step (d, c) (Just target) = step (nextDay, nc) nextTarget
  where
    nc = insertPath d target c
    nextDay = ((/= target) . snd) `filter` d
    nextTarget = chooseNext nextDay
step (d, c) Nothing = (d, c)

part1 :: Day -> Int
part1 day = total
  where
    (total, _, _) = c Map.! "you"
    (_, c) = step (day, startCounts) startTarget
    startTarget = chooseNext day
    startCounts = Map.insert "out" (1, 0, 0) $ Map.fromList $ (,(0, 0, 0)) <$> (snd <$> day) ++ (fst <$> day)

-- Part 2
part2 :: Day -> Int
part2 day = total
  where
    (_, _, total) = c Map.! "svr"
    (_, c) = step (day, startCounts) startTarget
    startTarget = chooseNext day
    startCounts = Map.insert "out" (1, 0, 0) $ Map.fromList $ (,(0, 0, 0)) <$> (snd <$> day) ++ (fst <$> day)
