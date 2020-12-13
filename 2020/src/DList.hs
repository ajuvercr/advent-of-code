module DList where

import Control.Monad.State.Lazy
import Debug.Trace

data DListState a = DListState { cidx  :: Int
                     , front :: [a]
                     , back  :: [a]
                     } deriving (Show)

type DList a = State (DListState a)

runDList2d :: DList (DListState a) b -> [[a]] -> (b, DListState (DListState a))
runDList2d l a = runState l $ fromList $ map fromList a

runDList :: DList a b -> [a] -> b
runDList l a = x
    where (x, _) = runState l $ fromList a

runDList' :: DList a b -> [a] -> (b, DListState a)
runDList' l a = runState l $ fromList a

fromList :: [a] -> DListState a
fromList = DListState 0 []


index' :: Show a => Int -> DList a a
index' i = do
    s <- get
    let (o, s') = index'' s i
    put s'
    return o


foldWith' :: (c -> DList a d) -> (d -> b -> b) -> b -> [c] -> DList a b
foldWith' _ _ b [] = return b
foldWith' g f b (c:cs) = do
    a <- g c
    foldWith' g f (f a b) cs


fold' :: Show a => (a -> b -> b) -> b -> [Int] -> DList a b
fold' = foldWith' index'


index'' :: Show a => DListState a -> Int -> (a, DListState a)
index'' s@DListState{cidx=cidx, front=f, back=b} i
    | i < cidx  = index'' (DListState (cidx-1) (tail f) (z f b)) i
    | i > cidx  = index'' (DListState (cidx+1) (z b f) (tail b)) i
    | otherwise = (head b, s)
    where z (x:_) xs = x:xs


map' :: Show a => [Int] -> DList a [a]
map' = fold' (:) []


update' :: Show a => (a -> (b, a)) -> Int -> DList a b
update' f i = do
                a <- index' i
                let (o, a') = f a
                modify $ bb a'
                return o
    where bb b s@DListState{back=(_:bs)} = s{back=b:bs}


get2d' :: Show a => (Int, Int) -> DList (DListState a) a
get2d' (x, y) = update' (`index''` y) x

fold2d' :: Show a => (a -> b -> b) -> b -> [(Int, Int)] -> DList (DListState a) b
fold2d' = foldWith' get2d'

map2d' :: Show a => [(Int, Int)] -> DList (DListState a) [a]
map2d' = fold2d' (:) []