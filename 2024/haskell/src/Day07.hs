{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day07 (parseDay, part1, part2) where

import qualified Data.Set as Set
import NanoParsec

data Test = Test
  { result :: Integer,
    parts :: [Integer]
  }
  deriving (Show)

data Op = Add | Times | Concat

cc :: Integer -> Integer -> Integer
cc a b = read (show a ++ show b)

applyOp :: (Foldable f, Monoid (f Integer)) => (Integer -> f Integer -> f Integer) -> [Op] -> Integer -> f Integer -> [Integer] -> f Integer
applyOp _ _ _ s [] = s
applyOp add os m s (b : xs) = applyOp add os m (foldl next mempty s) xs
  where
    next set x = foldl (apply x) set os

    apply a acc Add = tadd (a + b) acc
    apply a acc Times = tadd (a * b) acc
    apply a acc Concat = tadd (cc a b) acc

    tadd x acc | x <= m = add x acc
    tadd _ acc = acc

parseTest :: Parser Test
parseTest = Test <$> natural <* char ':' <*> plus (char ' ' *> natural)

--  Parsing
parseDay :: String -> [Test] -- adjust type per puzzle
parseDay = runParser $ star (parseTest <* char '\n')

filterTest :: [Op] -> Test -> Bool
filterTest os test = any isResult $ applyOp Set.insert os (result test) (Set.fromList [first]) rest
  where
    first : rest = parts test
    isResult x = x == result test

-- Part 1
part1 :: [Test] -> Integer
part1 tests = sum $ map result $ filter (filterTest [Times, Add]) tests

-- Part 2
part2 :: [Test] -> Integer
part2 tests = sum $ map result $ filter (filterTest [Times, Add, Concat]) tests
