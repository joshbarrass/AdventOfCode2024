module Main where

import Data.List
import ParseArgs
import Map

countTrue :: [Bool] -> Integer
countTrue = foldl' (\acc x -> if x then acc + 1 else acc) 0

main :: IO ()
main = do
  m <- loadInputFileLines :: IO Map
  let m' = filter (not . null) m
  let state = doGuard m'
  let v = visited state
  let c = foldl' (\acc x -> acc + countTrue x) 0 v
  print c
