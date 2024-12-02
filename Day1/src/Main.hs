module Main where

import ParseArgs
import Data.List.Split
import qualified Data.Map as Map

parseLine :: String -> (Integer, Integer)
parseLine x = let
  [a, b] = splitOn "   " x
  in (read a, read b)

parseLines :: [String] -> [(Integer, Integer)]
parseLines = map parseLine

getCountsMap :: [Integer] -> Map.Map Integer Integer
getCountsMap [] = Map.empty
getCountsMap (x:xs)
  | x `Map.member` rest = Map.update (\y -> Just (y+1)) x rest
  | otherwise = Map.insert x 1 rest
  where rest = getCountsMap xs

getCount :: Map.Map Integer Integer -> Integer -> Integer
getCount m x
  | x `Map.member` m = m Map.! x
  | otherwise = 0

sim :: Map.Map Integer Integer -> Integer -> Integer
sim m x = x * (getCount m x)

main :: IO ()
main = do
  l <- loadInputFileLines
  let parsed = parseLines l
  -- let parsed = [(3,4), (4,3), (2,5), (1,3), (3,9), (3,3)]
  let (x, y) = unzip parsed
  let counts = getCountsMap y
  let sims = map (sim counts) x
  let total = sum sims
  print total
