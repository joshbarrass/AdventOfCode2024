module Main where

import ParseArgs
import Control.Monad.State.Lazy

type SafetyState = State Integer

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
isSafe x = evalState (safetyFunc x) 0

main :: IO ()
main = do
  inp <- loadInputFileBySpaces
  let parsed = map (map read) inp :: [[Integer]]
  let safety = map isSafe parsed :: [Bool]
  print $ length $ filter id safety
