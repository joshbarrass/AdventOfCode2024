module Main where

import ParseArgs
import Data.List.Split
import Data.List

parseLine :: String -> (Integer, Integer)
parseLine x = let
  [a, b] = splitOn "   " x
  in (read a, read b)

parseLines :: [String] -> [(Integer, Integer)]
parseLines = map parseLine

distance :: Integer -> Integer -> Integer
distance x y = abs (x - y)

main :: IO ()
main = do
  l <- loadInputFileLines
  let parsed = parseLines l
  let (x, y) = unzip parsed
  let diffs = zipWith distance (sort x) (sort y)
  let total = sum diffs
  print total
