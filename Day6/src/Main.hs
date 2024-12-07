module Main where

import Data.List
import ParseArgs
import Map
import Grid

countTrue :: [Bool] -> Integer
countTrue = foldl' (\acc x -> if x then acc + 1 else acc) 0

countNotNull :: [[a]] -> Integer
countNotNull = foldl' (\acc x -> if null x then acc else acc + 1) 0

-- takes a map and inserts an object at the coordinate
-- returns True if the object causes a loop
testWithObject :: Map -> Coord -> Bool
testWithObject m c
  | x == '^' || x == '#' = False
  | otherwise = let m' = set m c '#' in not . fst $ doGuard m'
  where x = m !.! c

getAllCoords :: Grid a -> [Coord]
getAllCoords m = [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)]]
  where w = width m
        h = height m

getVisitedCoords :: VisitMap -> [Coord]
getVisitedCoords v = filter (not . null . (v !.!)) (getAllCoords v)

main :: IO ()
main = do
  m <- loadInputFileLines :: IO Map
  let m' = filter (not . null) m

  -- figure out which coordinates the guard could visit with their normal route
  let (_, WalkState _ _ v) = doGuard m'
  let cs = getVisitedCoords v

  print $ length $ getAllCoords m'
  print $ length cs

  -- do tests only at those coordinates the guard could hit
  let tests = map (testWithObject m') cs
  let possibleLoops = countTrue tests
  print possibleLoops
