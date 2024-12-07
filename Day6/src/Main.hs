module Main where

import Data.List
import ParseArgs
import Map
import Grid

import Data.Vector
import Control.Parallel.Strategies

countTrue :: [Bool] -> Integer
countTrue = Data.List.foldl' (\acc x -> if x then acc + 1 else acc) 0

countNotNull :: [[a]] -> Integer
countNotNull = Data.List.foldl' (\acc x -> if Prelude.null x then acc else acc + 1) 0

-- takes a map and inserts an object at the coordinate
-- returns True if the object causes a loop
testWithObject :: Map -> Coord -> Bool
testWithObject m c
  | x == '^' || x == '#' = False
  | otherwise = not . fst $ doGuardWithObject m c
  where x = m !.! c

getAllCoords :: Grid a -> [Coord]
getAllCoords m = [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)]]
  where w = width m
        h = height m

getVisitedCoords :: VisitMap -> [Coord]
getVisitedCoords v = Prelude.filter (not . Prelude.null . get) (getAllCoords v)
  where get = (v !.!)

main :: IO ()
main = do
  m <- loadInputFileLines :: IO [[Char]]
  let m' = fromList $ Prelude.map fromList $ Prelude.filter (not . Prelude.null) m :: Map

  -- figure out which coordinates the guard could visit with their normal route
  let (_, WalkState _ _ v) = doGuard m'
  let cs = getVisitedCoords v

  print $ Prelude.length $ getAllCoords m'
  print $ Prelude.length cs

  -- do tests only at those coordinates the guard could hit
  -- let tests = Prelude.map (testWithObject m') cs
  let tests = parMap rdeepseq (testWithObject m') cs
  let possibleLoops = countTrue tests
  print possibleLoops
