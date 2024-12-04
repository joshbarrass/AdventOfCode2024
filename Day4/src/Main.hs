module Main where

import Data.List
import ParseArgs
import Wordsearch

isXMAS :: Wordsearch -> Coord -> Bool
isXMAS ws c
  | ws !.! c /= 'A' = False
  | otherwise = and toCheck
  where (x, y) = c
        diag1 = ldtake ws (x+1, y-1) 3
        diag2 = rdtake ws (x-1, y-1) 3
        toCheck =
          [diag1 == "MAS" || diag1 == "SAM"
          ,diag2 == "MAS" || diag2 == "SAM"]

main :: IO ()
main = do
  ws <- loadInputFileLines :: IO Wordsearch
  let h = height ws
  let w = width ws
  let coords = [(x,y) | x <- [1..(w-2)], y <- [1..(h-2)]]
  let n = foldl' (\acc c -> if (isXMAS ws c) then acc + 1 else acc) 0 coords :: Integer
  print n
