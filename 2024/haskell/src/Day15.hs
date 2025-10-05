{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day15 (parseDay, part1, part2) where

import Control.Lens hiding (element)
import Control.Monad.State (State, get, put, runState)
import Data.Foldable (foldlM)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Set (Set, delete, empty, fromList, insert, member, toAscList)

type Coord = (Int, Int)

data St = St
  { _movable :: Set Coord,
    _left :: Set Coord,
    _right :: Set Coord,
    _static :: Set Coord,
    _location :: Set Coord
  }
  deriving (Show)

makeLensesFor [("_movable", "movable"), ("_left", "left"), ("_right", "right"), ("_location", "location")] ''St

data Move = MUp | MLeft | MDown | MRight deriving (Show, Eq)

readStatic :: String -> Set Coord
readStatic = readStatic' '#' insert empty (0, 0)

readMovable :: String -> Set Coord
readMovable = readStatic' 'O' insert empty (0, 0)

readRight :: String -> Set Coord
readRight = readStatic' 'O' insertRight empty (0, 0)
  where
    insertRight (i, j) s = (i * 2 + 1, j) `insert` s

readLeft :: String -> Set Coord
readLeft = readStatic' 'O' insertLeft empty (0, 0)
  where
    insertLeft (i, j) s = (i * 2, j) `insert` s

readPlayer :: String -> Maybe Coord
readPlayer = readStatic' '@' (\x _ -> Just x) Nothing (0, 0)

readStatic' :: Char -> (Coord -> a -> a) -> a -> Coord -> String -> a
readStatic' _ _ a _ [] = a
readStatic' c f a (_, j) ('\n' : xs) = readStatic' c f a (0, j + 1) xs
readStatic' c f a (i, j) (x : xs)
  | x == c = f (i, j) $ readStatic' c f a (i + 1, j) xs
  | otherwise = readStatic' c f a (i + 1, j) xs

readMoves :: String -> [Move]
readMoves = concatMap readMove
  where
    readMove '<' = [MLeft]
    readMove '>' = [MRight]
    readMove '^' = [MUp]
    readMove 'v' = [MDown]
    readMove _ = []

parseDay :: String -> (St, Coord, [Move]) -- adjust type per puzzle
parseDay st = (St m l r s empty, p, moves)
  where
    m = readMovable st
    s = readStatic st
    r = readRight st
    l = readLeft st
    p = fromJust $ readPlayer st
    moves = readMoves st

moveIt :: Move -> Coord -> Coord
moveIt MUp (x, y) = (x, y - 1)
moveIt MDown (x, y) = (x, y + 1)
moveIt MLeft (x, y) = (x - 1, y)
moveIt MRight (x, y) = (x + 1, y)

rollbackIfFalse :: State s Bool -> State s Bool
rollbackIfFalse action = do
  s <- get
  result <- action
  if result then pure True else put s >> pure False

doMove :: Lens' St (Set Coord) -> Move -> Coord -> State St Bool
doMove len move current = do
  St m l r s _ <- get
  let target = moveIt move current
  let isStatic = target `member` s
  let isBox = target `member` m
  let isLeft = target `member` l
  let isRight = target `member` r
  case (isStatic, isBox, isLeft, isRight, (move == MUp) || (move == MDown)) of
    (False, False, False, False, _) -> doInsert
    (True, _, _, _, _) -> return False
    (_, True, _, _, _) -> doMove movable move target >>= doLater
    -- Moving up or down, we have to check both sides
    (_, _, True, False, True) -> rollbackIfFalse ((&&) <$> doMove left move target <*> doMove right move (rightOf target)) >>= doLater
    (_, _, _, True, True) -> rollbackIfFalse ((&&) <$> doMove left move (leftOf target) <*> doMove right move target) >>= doLater
    -- Moving left or right, only look at the currect side
    (_, _, True, False, False) -> doMove left move target >>= doLater
    (_, _, _, True, False) -> doMove right move target >>= doLater
  where
    doLater :: Bool -> State St Bool
    doLater True = doInsert
    doLater False = pure False
    doInsert :: State St Bool
    doInsert = modifying len addCurrent $> True
    addCurrent :: Set Coord -> Set Coord
    addCurrent s = moveIt move current `insert` (current `delete` s)
    leftOf :: Coord -> Coord
    leftOf (i, j) = (i - 1, j)
    rightOf :: Coord -> Coord
    rightOf (i, j) = (i + 1, j)

applyMove :: Coord -> Move -> State St Coord
applyMove coord move = nextCoord <$> doMove location move coord
  where
    nextCoord True = moveIt move coord
    nextCoord False = coord

-- Part 2
part1 :: (St, Coord, [Move]) -> Int
part1 (St m _ _ s _, loc, moves) = sum $ map price $ toAscList movables
  where
    price (i, j) = i + 100 * j
    (_, St movables _ _ _ _) = runState moveMonad $ St m empty empty s empty -- empty out the state of the left and right
    moveMonad = foldlM applyMove loc moves

-- Part 2
part2 :: (St, Coord, [Move]) -> Int
part2 (St _ l r s _, (x, y), moves) = sum $ map price $ toAscList l'
  where
    price (i, j) = i + 100 * j
    (_, St _ l' _ _ _) = runState moveMonad $ St empty l r newStatic empty -- empty out the state of the movables
    moveMonad = foldlM applyMove (x * 2, y) moves
    newStatic = fromList $ concatMap dubble $ toAscList s
    dubble (i, j) = [(i * 2, j), (i * 2 + 1, j)]
