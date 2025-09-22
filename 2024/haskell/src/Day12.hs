module Day12 (parseDay, part1, part2) where

import Control.Monad.State
import Data.Map (Map, delete, insert, map, (!), (!?))

type Day = [Char]

type Coord = (Int, Int)

data Region = Region
  { area :: Int,
    perimeter :: Int
  }
  deriving (Show)

data St = St
  { look :: Map (Char, Coord) Int,
    regions :: Map Int Region,
    at :: Int
  }
  deriving (Show)

new :: Region
new = Region 0 0

getKey :: Char -> Coord -> MS (Maybe Int)
getKey i c = gets ((!? (i, c)) . look)

setKey :: Char -> Coord -> Int -> MS ()
setKey i c k = do
  St l r a <- get
  let nl = insert (i, c) k l
  put $ St nl r a

getRegion :: Int -> MS Region
getRegion x = gets ((! x) . regions)

newKey :: Char -> Coord -> MS Int
newKey i c = do
  St l r a <- get
  let nl = insert (i, c) (a + 1) l
  let nr = insert (a + 1) new r
  put $ St nl nr (a + 1)
  return (a + 1)

setRegion :: Int -> Region -> MS ()
setRegion i c = modify (\(St l r a) -> St l (insert i c r) a)

parseDay :: String -> Day -- adjust type per puzzle
parseDay x = x

type MS a = State St a

combineRegions :: Region -> Region -> Region
combineRegions (Region a1 p1) (Region a2 p2) = Region (a1 + a2) (p1 + p2)

combineKeys :: Int -> Int -> MS Int
combineKeys x y = do
  St l r a <- get
  let l2 = Data.Map.map mapKey l
  let r1 = r ! x
  let r2 = r ! y
  let nr = delete y $ insert x (combineRegions r1 r2) r
  put $ St l2 nr a
  return x
  where
    mapKey i | i == y = x
    mapKey i = i

perim :: Maybe Int -> Int
perim (Just _) = 2
perim _ = 0

getKey' :: Char -> Coord -> Maybe Int -> Maybe Int -> MS Int
getKey' _ _ (Just k1) (Just k2) | k1 == k2 = return k1
getKey' _ _ (Just k1) (Just k2) = combineKeys k1 k2
getKey' _ _ (Just k1) Nothing = return k1
getKey' _ _ Nothing (Just k1) = return k1
getKey' i c Nothing Nothing = newKey i c

parseYard :: Coord -> Char -> MS ()
parseYard (x, y) c = do
  k_up <- key up
  k_left <- key left
  k <- getKey' c (x, y) k_up k_left
  setKey c (x, y) k
  Region a p <- getRegion k
  setRegion k (Region (a + 1) (p + 4 - perim k_up - perim k_left))
  where
    up = (x - 1, y)
    left = (x, y - 1)
    key = getKey c

applyYard :: Coord -> [Char] -> MS ()
applyYard _ [] = pure ()
applyYard (_, y) ('\n' : xs) = applyYard (0, y + 1) xs
applyYard (x, y) (z : xs) = parseYard (x, y) z >> applyYard (x + 1, y) xs

price :: Region -> Int
price (Region x y) = x * y

-- Part 2
part1 :: [Char] -> Int
part1 day = foldr price' 0 (regions st)
  where
    price' r x = x + price r
    st = execState (applyYard (0, 0) day) (St mempty mempty 0)

-- Part 2
part2 :: [Char] -> Int
part2 day = 0
