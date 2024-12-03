module Main where

import ParseArgs
import Parser

main :: IO ()
main = do
  inp <- loadInputFile
  -- print inp
  let ins = parseAllIns inp
  let muls = getEnabledMul ins
  let res = sum $ map (uncurry (*)) muls
  print res
