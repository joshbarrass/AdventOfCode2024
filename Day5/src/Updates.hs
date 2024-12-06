module Updates (
   parseUpdate
  ,isValid
  ,middle
  ,fix
               ) where

import Data.List
import Data.List.Split
import Data.Maybe
import Rules

parseUpdate :: String -> [Integer]
parseUpdate = map read . splitOn ","

isValid :: [Rule] -> [Integer] -> Bool
isValid rs up = all (testRule up) rs

middle :: [Integer] -> Integer
middle xs = xs !! (l `div` 2)
  where l = length xs :: Int

fix :: [Rule] -> [Integer] -> [Integer]
fix _ [] = []
fix _ [x] = [x]
fix rs (x:xs)
  | isValid rs (x:xs') = x:xs' -- already satisfies the rules; good to go!
  -- moving the current head down will not violate any of the rules already satisfied
  -- move the head down until we find the first example that satisfies all the rules again
  | otherwise = let perms = [take i xs' ++ x : drop i xs' | i <- [0..]]
                in fromJust $ find (isValid rs) perms
  where xs' = fix rs xs
