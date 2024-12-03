module Parser (
  parseAllMul
              ) where

import Text.ParserCombinators.ReadP

-- define a parser that gives the OK when we see "mul("
mulStart :: ReadP ()
mulStart = string "mul(" >> return ()

-- define a parser that reads digits until there are no more, then
-- returns the number
numRead :: ReadP Integer
numRead = do
  numStr <- munch1 (flip elem "0123456789")
  return $ read numStr

-- define a parser that reads a full mul expression
mulExpr :: ReadP (Integer, Integer)
mulExpr = do
  mulStart
  a <- numRead
  _ <- char ','
  b <- numRead
  _ <- char ')'
  return (a,b)

-- Define a parser that skips until a valid mul expression is found
skipToValidMul :: ReadP (Integer, Integer)
skipToValidMul = mulExpr <++ (get >> skipToValidMul)

-- parser that finds all valid mul expressions in a file, ignoring any
-- garbage in between
allMul :: ReadP [(Integer, Integer)]
allMul = do
  mul <- many skipToValidMul
  return mul

-- user-facing function to run the parser
parseAllMul :: String -> [(Integer, Integer)]
parseAllMul contents = (fst . last) allResults
  where parser = readP_to_S (allMul)
        allResults = parser contents
        
