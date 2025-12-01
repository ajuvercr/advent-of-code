module Main (main) where

import System.Environment (getArgs)
import Days (days, runDay)


argsToInp :: [String] -> Maybe (String,  Maybe FilePath)
argsToInp [] = Nothing
argsToInp [day] = Just (day, Nothing) 
argsToInp (day:path:_) = Just (day, Just path) 

main :: IO ()
main = do
  args <- argsToInp <$> getArgs
  case args of
    Just (day, fp) -> case lookup day days of
      Just someDay -> runDay fp someDay
      Nothing -> putStrLn "Unknown day"
    _ -> putStrLn "Usage: stack run <day> [input]"

