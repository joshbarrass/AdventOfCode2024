module Parser (
  parseAllIns
 ,getEnabledMul
 ,Instruction(..) 
              ) where

import Text.ParserCombinators.ReadP
import qualified Control.Monad.State.Lazy as State

data Instruction = Mul (Integer, Integer) | Do | Dont

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
mulExpr :: ReadP Instruction
mulExpr = do
  mulStart
  a <- numRead
  _ <- char ','
  b <- numRead
  _ <- char ')'
  return $ Mul (a,b)

-- define a parser that reads a do instruction
doExpr :: ReadP Instruction
doExpr = string "do()" >> return Do

-- define a parser that reads a don't instruction
dontExpr :: ReadP Instruction
dontExpr = string "don't()" >> return Dont

-- Define a parser that skips until a valid instruction is found
skipToValidIns :: ReadP Instruction
skipToValidIns = (mulExpr +++ doExpr +++ dontExpr)
  <++ (get >> skipToValidIns)

-- parser that finds all valid instructions in a file, ignoring any
-- garbage in between
allIns :: ReadP [Instruction]
allIns = do
  ins <- many skipToValidIns
  return ins

-- user-facing function to run the parser
parseAllIns :: String -> [Instruction]
parseAllIns contents = (fst . last) allResults
  where parser = readP_to_S (allIns)
        allResults = parser contents
        
enabledMul :: [Instruction] -> State.State Bool [(Integer, Integer)]
enabledMul [] = return []
enabledMul (Do:xs) = State.put True >> enabledMul xs
enabledMul (Dont:xs) = State.put False >> enabledMul xs
enabledMul ((Mul x):xs) = do
  enabled <- State.get
  case enabled of True -> do
                    ys <- enabledMul xs
                    return $ x : ys
                  False -> enabledMul xs

getEnabledMul :: [Instruction] -> [(Integer, Integer)]
getEnabledMul xs = State.evalState (enabledMul xs) True
