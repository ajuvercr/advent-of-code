module Day02
    (solve
    ) where

import Lib
import NanoParsec

data Entry = Entry { pmin :: Int
                   , pmax :: Int
                   , t :: Char
                   , password :: String } deriving (Show)


parseEntry :: Parser Entry
parseEntry = do
    min <- token number
    token $ char '-'
    max <- token number
    t <- token item
    token $ char ':'
    pass <- str
    return Entry { pmin=min, pmax=max, t=t, password=pass }


parseEntries :: String -> [Entry]
parseEntries = map (runParser parseEntry) . lines


iscorrect :: Entry -> Bool
iscorrect Entry {pmin=min, pmax=max, t=t, password=pw} = min <= c && c <= max
    where c = count (==t) pw


xor :: Bool -> Bool -> Bool
xor True  True  = False
xor False False = False
xor _     _     = True


iscorrect2 :: Entry -> Bool
iscorrect2 Entry {pmin=min, pmax=max, t=t, password=pw} = xor (t == pw !! (min - 1)) (t == pw !! (max - 1))


solve :: String -> IO ()
solve x = do
    putStr "Part 1: "
    print $ count iscorrect $ parseEntries x
    putStr "Part 2: "
    print $ count iscorrect2 $ parseEntries x