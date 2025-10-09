{-# LANGUAGE RankNTypes #-}

module Day19 (parseDay, part1, part2) where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Control.Monad.State (MonadState (get), State, evalState, modify)
import Data.Functor (($>))
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
import NanoParsec (oneOf, plus, runParser, spaces, star, string)

type Day = ([String], [String])

type St a = Map String a

parseDay :: String -> Day
parseDay = runParser $ (,) <$> plus possible <* spaces <*> star wanted
  where
    possible = plus (oneOf "wubrg") <* (string ", " <|> pure ())
    wanted = plus (oneOf "wubrg") <* spaces

safe :: String -> a -> State (St a) a
safe k a = modify (Map.insert k a) $> a

isPossible :: a -> ([a] -> a) -> [String] -> String -> State (St a) a
isPossible base _ _ [] = pure base
isPossible base f available st = do
  state <- get
  case state Map.!? st of
    Just x -> return x
    Nothing -> maybed >>= safe st . f
  where
    tryFind o = traverse (isPossible base f available) (stripPrefix o st)
    things = Prelude.mapM tryFind available
    maybed = Data.Maybe.catMaybes <$> things

part1 :: Day -> Int
part1 (av, b) = length $ evalState parts Map.empty
  where
    parts = filterM (isPossible True or av) b

part2 :: Day -> Int
part2 (av, b) = sum $ evalState parts Map.empty
  where
    parts = Prelude.mapM (isPossible 1 sum av) b
