module Equations (
   Operator(..)
  ,Equation 
  ,getOperators
  ,evaluateEquation
  ,equationCanBeValid
                 ) where

import Data.Bits
import Data.List

data Operator = Add | Mul | Cat deriving (Show, Eq)
type Equation = ([Integer], [Operator])

getOperator :: Integer -> Operator
getOperator 0 = Add
getOperator 1 = Mul
getOperator 2 = Cat

evaluateOperator :: Operator -> Integer -> Integer -> Integer
evaluateOperator Add a b = a + b
evaluateOperator Mul a b = a * b
evaluateOperator Cat a b = read (show a ++ show b)

base :: Integer
base = 3

-- get least significant digit in base
-- args:
--  * base
--  * number
-- returns: (remainder, digit)
getLSDinBase :: Integer -> Integer -> (Integer, Integer)
getLSDinBase base number = number `divMod` base

getOperators :: Integer -> Integer -> [Operator]
getOperators 0 _ = []
getOperators n i
  | (toInteger . length) ys == n = ys
  | otherwise = getOperator d : ys
  where (r, d) = getLSDinBase base i
        ys = getOperators (n-1) r

evaluateEquation :: Equation -> Integer
evaluateEquation (n:ns, os) = foldl' (\acc (op, b) -> evaluateOperator op acc b) n (zip os ns)

maxi :: Integer -> Integer
maxi = (base^)

equationCanBeValid :: [Integer] -> Integer -> Bool
equationCanBeValid ns testval = any ((== testval) . evaluateEquation) [(ns, getOperators n i) | i <- init [0..(maxi n)]]
  where n = (toInteger . length) ns
