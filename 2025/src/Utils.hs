{-# LANGUAGE TupleSections #-}

module Utils ((?:), splitLast, traceOutput, traceInput, pairs, split, chunksOf) where

import Debug.Trace (trace)

(?:) :: Maybe a -> a -> a
Just a ?: _ = a
Nothing ?: b = b

infixr 0 ?:

-- Wrap a function to trace its result
traceOutput :: (Show b) => String -> (a -> b) -> (a -> b)
traceOutput st f x =
  let y = f x
   in trace (st ++ " " ++ show y) y

traceInput :: (Show a) => String -> (a -> b) -> (a -> b)
traceInput st f x = f $ trace (st ++ " " ++ show x) x

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x : xs) = items ++ pairs xs
  where
    items = map (x,) xs

split :: Char -> String -> [String]
split c s = case rest of
  [] -> [chunk]
  _ : rest' -> chunk : split c rest'
  where
    (chunk, rest) = break (== c) s

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

splitLast :: [a] -> ([a], a)
splitLast [] = error "splitLast: empty list"
splitLast [x] = ([], x)
splitLast (x : xs) =
  let (xs', lastx) = splitLast xs in (x : xs', lastx)
