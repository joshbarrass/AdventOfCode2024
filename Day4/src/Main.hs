module Main where

import Data.List
import ParseArgs
import Wordsearch

countTrue :: [Bool] -> Integer
countTrue = foldl' (\acc x -> if x then acc + 1 else acc) 0

-- check in all directions for an instance of the word and count them

-- the take functions are all defined such that the word will always
-- come out in the correct orientation. E.g. if the word is written
-- forwards, +n will give you the word, and if the word is written
-- backwards, then -n will give you the word (rather than the word
-- backwards). So we simply need to check all of these options to see
-- if the word is here.
checkForWord :: String -> Wordsearch -> Coord -> Integer
checkForWord w ws c
  | ws !.! c /= head w = 0 -- first letter doesn't match, so we can immediately return
  | otherwise = countTrue options
  where l = length w
        options =
          [w == htake ws c l
          ,w == htake ws c (-l)
          ,w == vtake ws c l
          ,w == vtake ws c (-l)
          ,w == ldtake ws c l
          ,w == ldtake ws c (-l)
          ,w == rdtake ws c l
          ,w == rdtake ws c (-l)]

main :: IO ()
main = do
  ws <- loadInputFileLines :: IO Wordsearch
  let h = height ws
  let w = width ws
  let coords = [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)]]
  let n = foldl' (\acc c -> acc + (checkForWord "XMAS" ws c)) 0 coords
  print n
