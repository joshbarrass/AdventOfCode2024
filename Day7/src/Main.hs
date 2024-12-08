module Main where

import Data.List
import Data.List.Split
import ParseArgs
import Equations

parseLine :: String -> (Integer, [Integer])
parseLine x = ((read . init . head) xs, map read $ tail xs)
  where xs = splitOn " " x

main :: IO ()
main = do
  l <- loadInputFileLines
  let p = map parseLine l
  let calibrationResult = foldl' (\acc (t, ns) -> if equationCanBeValid ns t then acc + t else acc) 0 p
  print calibrationResult
