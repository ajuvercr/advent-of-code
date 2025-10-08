{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day17 (parseDay, part1, part2) where

import Control.Lens hiding (element)
import Control.Monad.State (State, execState)
import Data.Bits (Bits (xor))
import GHC.Base ((<|>))
import NanoParsec (char, number, runParser, spaces, star, string)

data St = St
  { _a :: Int,
    _b :: Int,
    _c :: Int,
    _output :: [Int]
  }
  deriving (Show)

type Day = (St, [Int])

makeLensesFor [("_a", "a"), ("_b", "b"), ("_c", "c"), ("_output", "output")] ''St

parseDay :: String -> Day
parseDay = runParser pd
  where
    pd = do
      ra <- string "Register A: " >> number <* spaces
      rb <- string "Register B: " >> number <* spaces
      rc <- string "Register C: " >> number <* spaces
      ins <- string "Program: " >> star (number <* maybeComma)
      return (St ra rb rc [], ins)
    maybeComma = char ',' <|> pure '.'

combo :: Int -> State St Int
combo 0 = pure 0
combo 1 = pure 1
combo 2 = pure 2
combo 3 = pure 3
combo 4 = use a
combo 5 = use b
combo 6 = use c
combo _ = error "invalid combo"

divPow :: Int -> Int -> Int
divPow y x = x `div` 2 ^ y

-- The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.)
-- The result of the division operation is truncated to an integer and then written to the A register.
opcode :: Int -> [Int] -> State St Int
opcode idx (0 : v : _) = (combo v >>= ((a %=) . divPow)) >> pure (idx + 2)
-- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
opcode idx (1 : v : _) = b %= xor v >> pure (idx + 2)
-- The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
opcode idx (2 : v : _) = combo v >>= (b .=) . (`mod` 8) >> pure (idx + 2)
-- The jnz instruction (opcode 3) does nothing if the A register is 0. However,
-- if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand;
-- if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
opcode idx (3 : v : _) = use a >>= (\va -> if va == 0 then pure (idx + 2) else pure v)
-- The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
opcode idx (4 : _ : _) = (use c >>= (b %=) . xor) >> pure (idx + 2)
-- The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)
opcode idx (5 : v : _) = (combo v >>= (output %=) . (:) . (`mod` 8)) >> pure (idx + 2)
-- The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)
opcode idx (6 : v : _) = (divPow <$> combo v <*> use a >>= (b .=)) >> pure (idx + 2)
-- The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.)
opcode idx (7 : v : _) = (divPow <$> combo v <*> use a >>= (c .=)) >> pure (idx + 2)
opcode _ _ = error "Invalid opcode"

execCodes :: [Int] -> Int -> State St ()
execCodes xs idx
  | idx >= length xs = pure ()
  | otherwise = opcode idx f >>= execCodes xs
  where
    f = drop idx xs

-- Part 2
-- too high 106516
part1 :: (St, [Int]) -> [Char]
part1 (o, ins) = tail output'
  where
    St _ _ _ outpot = execState (execCodes ins 0) o
    output' = foldr (\x y -> y ++ "," ++ show x) "" outpot

getA :: [Int] -> Int -> Int
getA [] x = x
getA (x : xs) y = getA xs (x + y * 8)

getA' :: [Int] -> Int
-- getA' x = getA x 0

getA' xs = getA xs 0

findNext :: Day -> [Int] -> [Int] -> [[Int]]
findNext _ [] o = [o]
findNext (o, ins) (target : xs) firstA = concatMap tryI [0 .. 8]
  where
    good [x] = x == target
    good (_ : xt) = good xt
    good _ = False
    tryI x
      | x >= 8 = []
      | good o' = findNext (o, ins) xs (firstA ++ [x])
      | otherwise = []
      where
        St _ _ _ o' = execState (a .= getA' (firstA ++ [x]) >> execCodes ins 0) o

part2 :: (St, [Int]) -> Int
part2 (o, ins) = m'
  where
    m' = minimum values
    values = Prelude.map fst valids
    valids = Prelude.filter lastCheck $ Prelude.map toOutput os
    lastCheck (_, ins') = reverse ins' == ins
    toOutput x = (getA' x, x')
      where
        St _ _ _ x' = execState (a .= getA' x >> execCodes ins 0) o
    -- l = foldl (\a' b' -> 8 * a' + b') 0 output'
    os = findNext (o, ins) (reverse ins) []

-- My Input
-- 2 4: B = A % 8 -- lowest thee bytes
-- 1 1: B = B XOR 1
-- 7 5: C = A / 2^B
-- 1 5: B = B XOR 5
-- 4 0: B = B XOR C
-- 0 3: A = A / 2^3
-- 5 5: out B % 8
-- 3 0: A == 0 ? END : JMP 0
--
-- output = B % 8
-- = (B XOR C) % 8
-- = (B XOR 5 XOR C) % 8
-- = (B XOR 5 XOR (A / 2 ^ B)) % 8
-- = (B XOR 1 XOR 5 XOR (A / 2 ^ (B XOR 1))) % 8
-- = ((A % 8) XOR 4 XOR (A >> ((A % 8) XOR 1))) % 8
--
