module ParseArgs (
   loadInputFile
  ,loadInputFileLines
  ,loadInputFileBySpaces
                 ) where

import System.Environment
import Data.List.Split

getInputFilename :: IO String
getInputFilename = fmap head getArgs

loadInputFile :: IO String
loadInputFile = do
  fp <- getInputFilename
  readFile fp

loadInputFileLines :: IO [String]
loadInputFileLines = fmap lines loadInputFile

loadInputFileBySpaces :: IO [[String]]
loadInputFileBySpaces = fmap (filter (not . null) . map (splitOn " ")) loadInputFileLines
