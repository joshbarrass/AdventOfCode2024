module Main where

import ParseArgs
import Parser

main :: IO ()
main = do
  inp <- loadInputFile
  -- print inp
  let muls = parseAllMul inp
  let res = sum $ map (uncurry (*)) muls
  print res
