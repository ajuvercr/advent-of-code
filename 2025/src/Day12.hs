module Day12 (parseDay, part1, part2) where

import Control.Monad.State
import Data.Functor (($>))
import Data.Map (Map, delete, insert, map, (!), (!?))

type Day = [Char]

type Coord = (Int, Int)

type Parts = Map Coord Char

buildParts :: String -> Parts
buildParts = buildParts' 0 0 mempty
  where
    buildParts' _ _ m [] = m
    buildParts' _ j m ('\n' : xs) = buildParts' 0 (j + 1) m xs
    buildParts' i j m (x : xs) = buildParts' (i + 1) j (insert (i, j) x m) xs

data Region = Region
  { area :: Int,
    perimeter :: Int,
    corners :: Int
  }
  deriving (Show)

data St = St
  { look :: Map (Char, Coord) Int,
    regions :: Map Int Region,
    at :: Int
  }
  deriving (Show)

shift :: Int -> [a] -> [a]
shift _ [] = []
shift 0 a = a
shift i (x : xs) = shift (i - 1) $ xs ++ [x]

range :: Int -> [Int]
range 0 = [0]
range i = i : range (i - 1)

addCorners' :: Parts -> [Char] -> MS ()
addCorners' parts st = addCorners parts (mx, my) (mx, my) (0, 0)
  where
    ls = lines st
    mx = length $ head ls
    my = length ls + 1

addCorners :: Parts -> Coord -> Coord -> Coord -> MS ()
addCorners parts _ (0, 0) (i, j) = addCorner parts (i, j)
addCorners parts (tx, ty) (0, y) (i, j) = addCorner parts (i, j) >> addCorners parts (tx, ty) (tx, y - 1) (0, j + 1)
addCorners parts (tx, ty) (x, y) (i, j) = addCorner parts (i, j) >> addCorners parts (tx, ty) (x - 1, y) (i + 1, j)

addCorner :: Parts -> Coord -> MS ()
addCorner parts (i, j) = mapM addCorner' usableCorners $> ()
  where
    addCorner' (c, coord) = do
      key <- getKey c coord
      setCorner key
    setCorner (Just key) = do
      (Region a b c) <- getRegion key
      setRegion key (Region a b (c + 1))
      return ()
    setCorner Nothing = return ()

    usableCorners = Prelude.concatMap usable actualCorners
    actualCorners = Prelude.filter isCorner' $ Prelude.map (`shift` cornersList) $ range 3
    isCorner' :: [(Maybe Char, Coord)] -> Bool
    isCorner' cs = isCorner (Prelude.map fst cs)
    cornersList = Prelude.map (\(dx, dy) -> (parts !? (i + dx, j + dy), (i + dx, j + dy))) [(-1, -1), (0, -1), (0, 0), (-1, 0)]
    usable (_ : (Just c, coord) : _) = [(c, coord)]
    usable _ = []

-- left target right
isCorner :: [Maybe Char] -> Bool
isCorner (left : target : right : under : _) = (same left target + same target right) /= 1 && (same left target + same target right + same target under) /= 3
  where
    same :: Maybe Char -> Maybe Char -> Int
    same x y | x == y = 1
    same _ _ = 0
isCorner _ = False

new :: Region
new = Region 0 0 0

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
combineRegions (Region a1 p1 c1) (Region a2 p2 c2) = Region (a1 + a2) (p1 + p2) (c1 + c2)

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
  Region a p cs <- getRegion k
  setRegion k (Region (a + 1) (p + 4 - perim k_up - perim k_left) cs)
  where
    up = (x - 1, y)
    left = (x, y - 1)
    key = getKey c

applyYard :: Parts -> Coord -> [Char] -> MS ()
applyYard _ _ [] = pure ()
applyYard parts (_, y) ('\n' : xs) = applyYard parts (0, y + 1) xs
applyYard parts (x, y) (z : xs) = parseYard (x, y) z >> applyYard parts (x + 1, y) xs

price :: Region -> Int
price (Region x y _) = x * y

price2 :: Region -> Int
price2 (Region x _ c) = x * c

-- Part 2
part1 :: [Char] -> Int
part1 day = foldr price' 0 (regions st)
  where
    price' r x = x + price r
    parts = buildParts day
    st = execState (applyYard parts (0, 0) day) (St mempty mempty 0)

-- Part 2
part2 :: [Char] -> Int
part2 day = foldr price' 0 (regions st)
  where
    price' r x = x + price2 r
    parts = buildParts day
    st = execState (applyYard parts (0, 0) day >> addCorners' parts day) (St mempty mempty 0)
