module Map (
   Map
   ,WalkState(..)
   ,doGuard
           ) where

import Data.List
import Control.Monad.State.Lazy
import Data.Maybe
import Grid

type Map = Grid Char
type VisitMap = Grid Bool

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

rotate :: Direction -> Direction
rotate Up = Map.Right
rotate Map.Right = Down
rotate Down = Map.Left
rotate Map.Left = Up

stepGuard :: Map -> State WalkState ()
stepGuard m = do
  leaving <- isLeaving m
  if leaving then return () else do
    state <- get
    let WalkState (x, y) d v = state
    wall <- isWall m
    if wall then do
      put $ WalkState (x, y) (rotate d) v
      stepGuard m
    else do
      case d of
           Up -> do
             let c' = (x, y-1)
             let v' = set v c' True
             put $ WalkState c' d v'
           Map.Right -> do
             let c' = (x+1, y)
             let v' = set v c' True
             put $ WalkState c' d v'
           Down -> do
             let c' = (x, y+1)
             let v' = set v c' True
             put $ WalkState c' d v'
           Map.Left -> do
             let c' = (x-1, y)
             let v' = set v c' True
             put $ WalkState c' d v'
      stepGuard m

doGuard :: Map -> WalkState
doGuard m = execState (stepGuard m) initState
  where startPos = fromJust (findGuard m)
        w = width m
        h = height m
        initVisited = set [[False | _ <- [0..(w-1)]] | _ <- [0..(h-1)]] startPos True
        initState = WalkState startPos Up initVisited
