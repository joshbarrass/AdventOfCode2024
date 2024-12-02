module Main where

import ParseArgs
import Control.Monad.State.Lazy
import Data.List

type SafetyState = State Integer

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i+1) xs

safetyFunc :: [Integer] -> SafetyState Bool
safetyFunc [] = return True
safetyFunc [_] = return True
safetyFunc (x:y:xs) = do
  s <- get
  when (s == 0) $ put diffDir
  let direction = if s == 0 then diffDir else signum s
  if diffDir /= direction then return False
  else if adiff >= 1 && adiff <= 3 then safetyFunc (y:xs)
  else return False

  where diff = y - x
        adiff = abs diff
        diffDir = signum diff

isSafe :: [Integer] -> Bool
isSafe x = any (doit . uncurry removeAt) iters
  where toRem = [0..(length x - 1)] :: [Int]
        iters = zip toRem (repeat x)
        doit xs = evalState (safetyFunc xs) 0

countTrue :: [Bool] -> Integer
countTrue = foldl' (\acc x -> if x then acc + 1 else acc) 0
      
main :: IO ()
main = do
  inp <- loadInputFileBySpaces
  let parsed = map (map read) inp :: [[Integer]]
  let safety = map isSafe parsed :: [Bool]
  print $ countTrue safety
