{-# LANGUAGE TupleSections #-}

module Utils ((?:), traceOutput, pairs) where

import Debug.Trace (trace)

(?:) :: Maybe a -> a -> a
Just a ?: _ = a
Nothing ?: b = b

infixr 0 ?:

-- Wrap a function to trace its result
traceOutput :: (Show b) => (a -> b) -> (a -> b)
traceOutput f x =
  let y = f x
   in trace ("Trace: " ++ show y) y

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x : xs) = items ++ pairs xs
  where
    items = map (x,) xs
