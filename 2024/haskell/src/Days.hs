{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Days (Day, days, runDay) where

import qualified Day01
import qualified Day02
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day24
import qualified Day25
import Utils ((?:))

-- … add more here
--
data Day a b c = Day
  { parse :: String -> a,
    part1 :: a -> b,
    part2 :: a -> c,
    input :: FilePath
  }

data SomeDay = forall a b c. (Show b, Show c) => SomeDay (Day a b c)

days :: [(String, SomeDay)]
days =
  [ ("1", SomeDay $ Day Day01.parse Day01.part1 Day01.part2 "inputs/day01.txt"),
    ("2", SomeDay $ Day Day02.parse Day02.part1 Day02.part2 "inputs/day02.txt"),
    ("6", SomeDay $ Day Day06.parseDay Day06.part1 Day06.part2 "../input/06.txt"),
    ("7", SomeDay $ Day Day07.parseDay Day07.part1 Day07.part2 "../input/day07.txt"),
    ("8", SomeDay $ Day Day08.parseDay Day08.part1 Day08.part2 "../input/day08.txt"),
    ("9", SomeDay $ Day Day09.parseDay Day09.part1 Day09.part2 "../input/day09.txt"),
    ("10", SomeDay $ Day Day10.parseDay Day10.part1 Day10.part2 "../input/10.txt"),
    ("11", SomeDay $ Day Day11.parseDay Day11.part1 Day11.part2 "../input/11.txt"),
    ("12", SomeDay $ Day Day12.parseDay Day12.part1 Day12.part2 "../input/12.txt"),
    ("13", SomeDay $ Day Day13.parseDay Day13.part1 Day13.part2 "../input/13.txt"),
    ("14", SomeDay $ Day Day14.parseDay Day14.part1 Day14.part2 "../input/14.txt"),
    ("15", SomeDay $ Day Day15.parseDay Day15.part1 Day15.part2 "../input/15.txt"),
    ("16", SomeDay $ Day Day16.parseDay Day16.part1 Day16.part2 "../input/16.txt"),
    ("17", SomeDay $ Day Day17.parseDay Day17.part1 Day17.part2 "../input/17.txt"),
    ("18", SomeDay $ Day Day18.parseDay Day18.part1 Day18.part2 "../input/18.txt"),
    ("19", SomeDay $ Day Day19.parseDay Day19.part1 Day19.part2 "../input/19.txt"),
    ("20", SomeDay $ Day Day20.parseDay Day20.part1 Day20.part2 "../input/20.txt"),
    ("21", SomeDay $ Day Day21.parseDay Day21.part1 Day21.part2 "../input/21.txt"),
    ("24", SomeDay $ Day Day24.parseDay Day24.part1 Day24.part2 "../input/24.txt"),
    ("25", SomeDay $ Day Day25.parseDay Day25.part1 Day25.part2 "../input/25.txt")
  ]

runDay :: Maybe FilePath -> SomeDay -> IO ()
runDay fp (SomeDay d) = do
  inputStr <- readFile $ fp ?: input d
  let parsed = parse d inputStr
  putStrLn $ "Part 1: " ++ show (part1 d parsed)
  putStrLn $ "Part 2: " ++ show (part2 d parsed)
