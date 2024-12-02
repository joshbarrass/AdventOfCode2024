module ParseArgs (
   loadInputFile
  ,loadInputFileLines
                 ) where

import System.Environment

getInputFilename :: IO String
getInputFilename = fmap head getArgs

loadInputFile :: IO String
loadInputFile = do
  fp <- getInputFilename
  readFile fp

loadInputFileLines :: IO [String]
loadInputFileLines = fmap lines loadInputFile
