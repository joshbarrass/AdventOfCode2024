module Main where

import Data.List.Split
import ParseArgs
import Rules
import Updates

getSections :: IO ([String], [String])
getSections = do
  l <- loadInputFileLines
  let [rules, updates] = take 2 $ splitOn [""] l
  return (rules, updates)

main :: IO ()
main = do
  (rawRules, rawUpdates) <- getSections
  let rules = map parseRule rawRules
  let updates = map parseUpdate rawUpdates
  let validUpdates = filter (isValid rules) updates
  let midpoints = map middle validUpdates
  print $ sum midpoints
