{-# LANGUAGE TupleSections #-}

module Utils ((?:), traceOutput, traceInput, pairs) where

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
