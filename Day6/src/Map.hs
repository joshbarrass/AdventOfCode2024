module Map (
   Map
   ,WalkState(..)
   ,VisitMap
   ,Coord
   ,width
   ,height
   ,doGuard
           ) where

import Data.List
import Control.Monad.State.Lazy
import Data.Maybe
import Grid

type Map = Grid Char
type VisitCell = [Direction]
type VisitMap = Grid VisitCell

data Direction = Up | Down | Left | Right deriving (Show, Eq)
data WalkState = WalkState { guardPos :: Coord, guardDir :: Direction, visited :: VisitMap }

findGuard :: Map -> Maybe Coord
findGuard m = do
  y <- findIndex (elem '^') m
  let row = m !-! y
  x <- elemIndex '^' row
  return (x, y)

isLeaving :: Map -> State WalkState Bool
isLeaving m = do
  WalkState (x, y) d _ <- get
  return $ or [x == 0 && d == Map.Left
            ,y == 0 && d == Up
            ,x == (width m - 1) && d == Map.Right
            ,y == (height m - 1) && d == Down]

isWall :: Map -> State WalkState Bool
isWall m = do
  WalkState (x, y) d _ <- get
  return $ or [d == Map.Left && m !.! (x-1, y) == '#'
            ,d == Up && m !.! (x, y-1) == '#'
            ,d == Map.Right && m !.! (x+1, y) == '#'
            ,d == Down && m !.! (x, y+1) == '#']

isLoop :: State WalkState Bool
isLoop = do
  WalkState c d v <- get
  return $ d `elem` tail (v !.! c)

rotate :: Direction -> Direction
rotate Up = Map.Right
rotate Map.Right = Down
rotate Down = Map.Left
rotate Map.Left = Up

stepGuard :: Map -> State WalkState Bool
stepGuard m = do
  looping <- isLoop
  if looping then return False else do
    leaving <- isLeaving m
    if leaving then return True else do
      state <- get
      let WalkState (x, y) d v = state
      wall <- isWall m
      if wall then do
        put $ WalkState (x, y) (rotate d) v
        stepGuard m
      else do
        let c' = case d of
                      Up -> (x, y-1)
                      Map.Right -> (x+1, y)
                      Down -> (x, y+1)
                      Map.Left -> (x-1, y)
        let v' = set v c' (d : v !.! c')
        put $ WalkState c' d v'
        stepGuard m

doGuard :: Map -> (Bool, WalkState)
doGuard m = runState (stepGuard m) initState
  where startPos = fromJust (findGuard m)
        w = width m
        h = height m
        initVisited = set [[[] | _ <- [0..(w-1)]] | _ <- [0..(h-1)]] startPos [Up]
        initState = WalkState startPos Up initVisited
