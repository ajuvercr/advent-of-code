{-# LANGUAGE RankNTypes #-}

module Day24 (parseDay, part1, part2) where

import Control.Applicative ((<|>))
import Data.List (sort)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Debug.Trace (trace)
import NanoParsec (anyChar, char, noneOf, runParser, spaces, star, string)

data Operator = Xor | And | Or deriving (Show, Eq)

data Op = Op
  { op :: Operator,
    a :: String,
    b :: String,
    o :: String
  }
  deriving (Show)

type Day = (Map String Bool, [Op])

parseDay :: String -> Day
parseDay = runParser parser
  where
    ident = star $ noneOf ": \n"
    v = (== '1') <$> anyChar
    s = (,) <$> ident <*> (string ": " *> v) <* char '\n'
    starting = Map.fromList <$> star s
    andParser = Op And <$> ident <* string " AND " <*> ident <* string " -> " <*> ident <* char '\n'
    orParser = Op Or <$> ident <* string " OR " <*> ident <* string " -> " <*> ident <* char '\n'
    xorParser = Op Xor <$> ident <* string " XOR " <*> ident <* string " -> " <*> ident <* char '\n'
    ops = star (andParser <|> orParser <|> xorParser)
    parser = (,) <$> starting <* spaces <*> ops

apply :: Map String Bool -> [Op] -> (Map String Bool, [Op])
apply s [] = (s, [])
apply s (Op op' a' b' o' : xs) = case (a'', b'') of
  (Just mx, Just my) -> (Map.insert o' (f op' mx my) s', ops')
  _ -> (s', Op op' a' b' o' : ops')
  where
    f Xor x y
      | x == y = False
      | otherwise = True
    f And x y = x && y
    f Or x y = x || y
    a'' = s !? a'
    b'' = s !? b'
    (s', ops') = apply s xs

applyAll :: Map String Bool -> [Op] -> Map String Bool
applyAll x xs
  | null xs' = x'
  | otherwise = applyAll x' xs'
  where
    (x', xs') = apply x xs

-- Part 2
part1 :: Day -> Int
part1 (x, xs) = foldr f 0 state'
  where
    f True x' = x' * 2 + 1
    f False x' = x' * 2
    state' = snd <$> filt `filter` Map.toAscList (applyAll x xs)
    filt ('z' : _, _) = True
    filt _ = False

data Op2 = N (Operator, Op2, Op2) | L String
  deriving (Show, Eq)

inString :: Int -> (String, String)
inString x
  | x < 10 = ("x0" ++ show x, "y0" ++ show x)
  | otherwise = ("x" ++ show x, "y" ++ show x)

oString :: Int -> String
oString x
  | x < 10 = "z0" ++ show x
  | otherwise = "z" ++ show x

order :: Op2 -> Op2
order (L x) = L x
order (N (o', l, r))
  | depth l < depth r = N (o', l', r')
  | otherwise = N (o', r', l')
  where
    l' = order l
    r' = order r

depth :: Op2 -> String
depth (L z) = z
depth (N (_, l, r)) = "'" ++ max (depth l) (depth r)

-- v i = XOR (carry i-1) (XOR xi yi)
-- carry i = OR (AND (carry i-1) (XOR xi yi)) (AND xi xi)

carry :: Int -> Op2
carry 0 = N (And, L "x00", L "y00")
carry i = N (Or, N (And, carry (i - 1), N (Xor, L x, L y)), N (And, L x, L y))
  where
    (x, y) = inString i

expected :: Int -> Op2
expected 0 = N (Xor, L "y00", L "x00")
expected i = N (Xor, carry (i - 1), N (Xor, L x, L y))
  where
    (x, y) = inString i

eql :: Op2 -> Op2 -> Bool
eql (L x) (L y) = x == y
eql (N (o1, x1, y1)) (N (o2, x2, y2))
  | o1 == o2 = (eql x1 x2 && eql y1 y2) || (eql x1 y2 && eql y1 x2)
  | otherwise = False
eql _ _ = False

-- wrongs: z08,vvr
-- kwv OR ctv -> z08
-- vvr XOR ggf -> z09
part2' :: [Op] -> String -> Op2
part2' _ "z08" = expected 8
part2' _ "vvr" = carry 8
part2' _ "rnq" = N (Xor, L "x16", L "y16") -- was N (And,L "x16",L "y16")
part2' _ "bkr" = N (And, L "x16", L "y16") -- was N (Xor,L "x16",L "y16")
part2' _ "z28" = expected 28 -- was (And,L "x28",L "y28")
part2' _ "tfb" = N (And, L "x28", L "y28") -- z28
-- part2' _ "z39" = expected 39
part2' ops "mqh" = part2' ops "z39"
part2' ops target = case from of
  Just (Op o' a' b' _) -> N (o', part2' ops a', part2' ops b')
  -- Just (Op o a b c) -> N (o, expected (asInt a), expected (asInt b))
  Nothing -> L target
  where
    foundInOp :: [Op] -> Maybe Op
    foundInOp [] = Nothing
    foundInOp (x : xs)
      | o x == target = Just x
      | otherwise = foundInOp xs
    from = foundInOp ops

-- Part 2
part2 :: Day -> [String]
part2 (_, ops) = trace (unlines built) (sort ["vvr", "z08", "rnq", "bkr", "z28", "tfb", "z39", "mqh"])
  where
    built = sti <$> [38 .. 40]
    sti x = oString x ++ ": " ++ show (eql p2 e) ++ "\n Got " ++ show (order p2) ++ "\n Exp " ++ show (order e)
      where
        p2 = part2' ops $ oString x
        e = expected x
