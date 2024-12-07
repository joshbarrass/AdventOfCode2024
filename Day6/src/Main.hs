module Main where

import Data.List
import ParseArgs
import Map

countNotNull :: [[a]] -> Integer
countNotNull = foldl' (\acc x -> if null x then acc else acc + 1) 0

main :: IO ()
main = do
  m <- loadInputFileLines :: IO Map
  let m' = filter (not . null) m
  let (exited, state) = doGuard m'
  print exited
  let v = visited state
  let c = foldl' (\acc x -> acc + countNotNull x) 0 v
  print c
