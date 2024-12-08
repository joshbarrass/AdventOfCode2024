module Equations (
   Operator(..)
  ,Equation 
  ,getOperators
  ,evaluateEquation
  ,equationCanBeValid
                 ) where

import Data.Bits
import Data.List

data Operator = Add | Mul deriving (Show, Eq)
type Equation = ([Integer], [Operator])

getOperator :: Integer -> Operator
getOperator 0 = Add
getOperator 1 = Mul

evaluateOperator :: Operator -> Integer -> Integer -> Integer
evaluateOperator Add a b = a + b
evaluateOperator Mul a b = a * b

getOperators :: Integer -> Integer -> [Operator]
getOperators 0 _ = []
getOperators n i
  | (toInteger . length) ys == n = ys
  | otherwise = getOperator (1 .&. i) : ys
  where ys = getOperators (n-1) (i `shiftR` 1)

evaluateEquation :: Equation -> Integer
evaluateEquation (n:ns, os) = foldl' (\acc (op, b) -> evaluateOperator op acc b) n (zip os ns)

maxi :: Integer -> Integer
maxi = (2^)

equationCanBeValid :: [Integer] -> Integer -> Bool
equationCanBeValid ns testval = any ((== testval) . evaluateEquation) [(ns, getOperators n i) | i <- init [0..(maxi n)]]
  where n = (toInteger . length) ns
