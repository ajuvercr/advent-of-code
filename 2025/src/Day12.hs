{-# LANGUAGE TupleSections #-}

module Day12 (parseDay, part1, part2) where

import NanoParsec (Parser, char, check, number, oneOf, runParser, star, string)

type Coord = (Int, Int)

type Shape = [Coord]

type Tree = (Coord, [Int])

type Day = ([Shape], [Tree])

parseShapeLine :: Parser (Int -> Shape)
parseShapeLine = do
  parts <- star (oneOf ".#") <* char '\n'
  let zipped = ((== '#') . fst) `filter` (parts `zip` [0 ..])
  return $ \i -> (i,) . snd <$> zipped

parseShape :: Parser Shape
parseShape = do
  _ <- number *> string ":\n"
  shapeLines <- star (check (/= '\n') *> parseShapeLine)
  return $ (\(f, i) -> f i) `concatMap` zip shapeLines [0 ..]

parseTree :: Parser Tree
parseTree = (,) <$> size <*> idx
  where
    size = (,) <$> number <* char 'x' <*> number <* char ':'
    idx = star $ char ' ' *> number

parseDay :: String -> Day -- adjust type per puzzle
parseDay = runParser parser
  where
    shapes = star $ parseShape <* char '\n'
    trees = star $ parseTree <* char '\n'
    parser = (,) <$> shapes <*> trees

fits :: [Shape] -> Tree -> Bool
fits sh ((w, h), counts) = required < size
  where
    size = w * h
    required = sum $ uncurry (*) <$> zip (length <$> sh) counts

-- Part 2
part1 :: Day -> Int
part1 (shapes, trees) = length $ fits shapes `filter` trees

-- Part 2
part2 :: Day -> Int
part2 _ = 2025
