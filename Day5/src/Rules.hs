module Rules (
   Rule
  ,parseRule
  ,testRule
             ) where

import Data.List.Split

type Rule = (Integer, Integer)

parseRule :: String -> Rule
parseRule x = let [a,b] = splitOn "|" x in (read a, read b)

testRule :: [Integer] -> Rule -> Bool
testRule [] _ = True
testRule [_] _ = True
testRule (x:xs) (a,b)
  | not r = False -- short circuit
  | b == x = a `notElem` xs
  | otherwise = True
  where r = testRule xs (a,b)
