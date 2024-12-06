module Updates (
   parseUpdate
  ,isValid
  ,middle
               ) where

import Data.List.Split
import Rules

parseUpdate :: String -> [Integer]
parseUpdate = map read . splitOn ","

isValid :: [Rule] -> [Integer] -> Bool
isValid rs up = all (testRule up) rs

middle :: [Integer] -> Integer
middle xs = xs !! (l `div` 2)
  where l = length xs :: Int
