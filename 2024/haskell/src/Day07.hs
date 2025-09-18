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

ops :: [Op] -> Integer -> Integer -> [Integer]
ops os a b = map op os
  where
    op Add = a + b
    op Times = a * b
    op Concat = read (show a ++ show b)

applyOp :: (Foldable f, Monoid (f Integer)) => ([Integer] -> f Integer) -> [Op] -> Integer -> f Integer -> [Integer] -> f Integer
applyOp _ _ _ s [] = s
applyOp fromList os m s (b : xs) = applyOp fromList os m (foldl next mempty s) xs
  where
    next set x = fromList (filter ltMax (ops os x b)) <> set
    ltMax x = x <= m

parseTest :: Parser Test
parseTest = Test <$> natural <* char ':' <*> plus (char ' ' *> natural)

--  Parsing
parseDay :: String -> [Test] -- adjust type per puzzle
parseDay = runParser $ star (parseTest <* char '\n')

filterTest :: [Op] -> Test -> Bool
filterTest os test = any isResult $ applyOp Set.fromList os (result test) (Set.fromList [first]) rest
  where
    first : rest = parts test
    isResult x = x == result test

-- Part 1
part1 :: [Test] -> Integer
part1 tests = sum $ map result $ filter (filterTest [Times, Add]) tests

-- Part 2
part2 :: [Test] -> Integer
part2 tests = sum $ map result $ filter (filterTest [Times, Add, Concat]) tests
