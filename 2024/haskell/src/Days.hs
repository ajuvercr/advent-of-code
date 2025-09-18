{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Days (Day, days, runDay) where

import qualified Day01
import qualified Day02
import qualified Day07
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
    ("7", SomeDay $ Day Day07.parseDay Day07.part1 Day07.part2 "inputs/day02.txt")
  ]

runDay :: Maybe FilePath -> SomeDay -> IO ()
runDay fp (SomeDay d) = do
  inputStr <- readFile $ fp ?: input d
  let parsed = parse d inputStr
  putStrLn $ "Part 1: " ++ show (part1 d parsed)
  putStrLn $ "Part 2: " ++ show (part2 d parsed)
