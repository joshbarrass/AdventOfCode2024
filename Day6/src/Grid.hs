module Grid (
    Grid
  , Coord
  , (!.!)
  , (!-!)
  , (!|!)
  , (!\!)
  , (!/!)
  , width
  , height
  , htake
  , vtake
  , ldtake
  , rdtake
  , set
                  ) where

import Data.Vector

type Grid a = Vector (Vector a)
type Coord = (Int, Int)

{-

 == Idea ==

Define take-like functions for getting a list of characters in a
particular direction.

For example:
htake: gets n characters along the horizontal
       n can be negative to read backwards along the horizontal
       e.g. if the row reads RABOOFOO
            if the coordinate is (5, 0) and n = 1  => F
                                            n = 3  => FOO
                                            n = -6 => FOOBAR
vtake: gets n characters along the vertical
       same convention as htake

ldtake: gets n characters going down along the left diagonal (south-west)
        negative n goes north-east

rdtake: gets n characters going down along the right diagonal (south-east)
        negative n goes north-west
-}

width :: Grid a -> Int
width = Data.Vector.length . (flip (!-!) 0)

height :: Grid a -> Int
height = Data.Vector.length

(!.!) :: Grid a -> Coord -> a
(!.!) ws (x, y) = (ws ! y) ! x

-- alias for consistency
(!-!) :: Grid a -> Int -> Vector a
(!-!) = (!)

-- gets a column
(!|!) :: Grid a -> Int -> Vector a
(!|!) ws x = fromList [ws !.! (x,y) | y <- [0..(h-1)]]
  where h = height ws

-- gets a diagonal going along \, from smallest x to biggest x
(!\!) :: Grid a -> Coord -> Vector a
(!\!) ws (x, y) = Data.Vector.map ((!.!) ws) $ Data.Vector.zip (fromList [minx..maxx]) (fromList [miny..maxy])
  where w = width ws
        h = height ws
        c = (x-y)
        minx = max 0 c
        miny = minx - c
        maxx = min (w-1) (h - 1 + c)
        maxy = maxx - c

indexrd :: Grid a -> Coord -> Int
indexrd _ (x, y) = x - minx
  where c = (x-y)
        minx = max 0 c

-- gets a diagonal going along /, from smallest x to biggest x
(!/!) :: Grid a -> Coord -> Vector a
(!/!) ws (x, y) = Data.Vector.map ((!.!) ws) $ Data.Vector.zip (fromList [minx..maxx]) (fromList (Prelude.reverse [miny..maxy]))
  where w = width ws
        h = height ws
        c = (x+y)
        minx = max 0 (c - h + 1)
        miny = c - maxx
        maxx = min (w-1) c
        maxy = c - minx

indexld :: Grid a -> Coord -> Int
indexld ws (x, y) = x - minx
  where h = height ws
        c = (x+y)
        minx = max 0 (c - h + 1)

htake :: Grid a -> Coord -> Int -> Vector a
htake _ _ 0 = fromList []
htake ws c 1 = fromList [ws !.! c]
htake ws c (-1) = fromList [ws !.! c]
htake ws (x, y) n
  | n > 0 = Data.Vector.take n $ Data.Vector.drop x row
  | otherwise = Data.Vector.reverse $ Data.Vector.take actuallyTake $ Data.Vector.drop (x+n+1) row
  where row = ws ! y
        actuallyTake = min (-n) (x+1)

vtake :: Grid a -> Coord -> Int -> Vector a
vtake _ _ 0 = fromList []
vtake ws c 1 = fromList [ws !.! c]
vtake ws c (-1) = fromList [ws !.! c]
vtake ws (x, y) n
  | n > 0 = Data.Vector.take n $ Data.Vector.drop y col
  | otherwise = Data.Vector.reverse $ Data.Vector.take actuallyTake $ Data.Vector.drop (y+n+1) col
  where col = ws !|! x
        actuallyTake = min (-n) (y+1)

ldtake :: Grid a -> Coord -> Int -> Vector a
ldtake _ _ 0 = fromList []
ldtake ws c 1 = fromList [ws !.! c]
ldtake ws c (-1) = fromList [ws !.! c]
ldtake ws c n
  | n > 0 = Data.Vector.reverse $ Data.Vector.take actuallyTake $ Data.Vector.drop (i-n+1) diag
  | otherwise = Data.Vector.take (-n) $ Data.Vector.drop i diag
  where diag = ws !/! c
        i = indexld ws c
        actuallyTake = min n (i+1)

rdtake :: Grid a -> Coord -> Int -> Vector a
rdtake _ _ 0 = fromList []
rdtake ws c 1 = fromList [ws !.! c]
rdtake ws c (-1) = fromList [ws !.! c]
rdtake ws c n
  | n > 0 = Data.Vector.take n $ Data.Vector.drop i diag
  | otherwise = Data.Vector.reverse $ Data.Vector.take actuallyTake $ Data.Vector.drop (i+n+1) diag
  where diag = ws !\! c
        i = indexrd ws c
        actuallyTake = min (-n) (i+1)

set :: Grid a -> Coord -> a -> Grid a
set old (x,y) item
  | (x < width old && x >= 0) && (y < height old && y >= 0) = Data.Vector.take y old Data.Vector.++ fromList [row'] Data.Vector.++ Data.Vector.drop (y+1) old
  | otherwise = old
  where row = old ! y
        row' = Data.Vector.take x row Data.Vector.++ fromList [item] Data.Vector.++ Data.Vector.drop (x+1) row
