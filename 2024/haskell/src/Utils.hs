module Utils ((?:), traceOutput) where

import Debug.Trace (trace)

(?:) :: Maybe a -> a -> a
Just a ?: b = a
Nothing ?: b = b

infixr 0 ?:

-- Wrap a function to trace its result
traceOutput :: (Show b) => (a -> b) -> (a -> b)
traceOutput f x =
  let y = f x
   in trace ("Trace: " ++ show y) y
