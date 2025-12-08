{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Days (Day, days, runDay) where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
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
  [ ("1", SomeDay $ Day Day01.parse Day01.part1 Day01.part2 "input/01.txt"),
    ("2", SomeDay $ Day Day02.parse Day02.part1 Day02.part2 "input/02.txt"),
    ("3", SomeDay $ Day Day03.parse Day03.part1 Day03.part2 "input/03.txt"),
    ("4", SomeDay $ Day Day04.parse Day04.part1 Day04.part2 "input/04.txt"),
    ("5", SomeDay $ Day Day05.parse Day05.part1 Day05.part2 "input/05.txt"),
    ("6", SomeDay $ Day Day06.parseDay Day06.part1 Day06.part2 "input/06.txt"),
    ("7", SomeDay $ Day Day07.parseDay Day07.part1 Day07.part2 "input/07.txt"),
    ("8", SomeDay $ Day Day08.parseDay Day08.part1 Day08.part2 "input/08.txt"),
    ("10", SomeDay $ Day Day10.parseDay Day10.part1 Day10.part2 "input/10.txt"),
    ("11", SomeDay $ Day Day11.parseDay Day11.part1 Day11.part2 "input/11.txt"),
    ("12", SomeDay $ Day Day12.parseDay Day12.part1 Day12.part2 "input/12.txt")
  ]

runDay :: Maybe FilePath -> SomeDay -> IO ()
runDay fp (SomeDay d) = do
  inputStr <- readFile $ fp ?: input d
  let parsed = parse d inputStr
  putStrLn $ "Part 1: " ++ show (part1 d parsed)
  putStrLn $ "Part 2: " ++ show (part2 d parsed)
