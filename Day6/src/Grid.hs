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

type Grid a = [[a]]
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
width = length . (flip (!-!) 0)

height :: Grid a -> Int
height = length

(!.!) :: Grid a -> Coord -> a
(!.!) ws (x, y) = (ws !! y) !! x

-- alias for consistency
(!-!) :: Grid a -> Int -> [a]
(!-!) = (!!)

-- gets a column
(!|!) :: Grid a -> Int -> [a]
(!|!) ws x = [ws !.! (x,y) | y <- [0..(h-1)]]
  where h = height ws

-- gets a diagonal going along \, from smallest x to biggest x
(!\!) :: Grid a -> Coord -> [a]
(!\!) ws (x, y) = map ((!.!) ws) $ zip [minx..maxx] [miny..maxy]
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
(!/!) :: Grid a -> Coord -> [a]
(!/!) ws (x, y) = map ((!.!) ws) $ zip [minx..maxx] (reverse [miny..maxy])
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

htake :: Grid a -> Coord -> Int -> [a]
htake _ _ 0 = []
htake ws c 1 = [ws !.! c]
htake ws c (-1) = [ws !.! c]
htake ws (x, y) n
  | n > 0 = take n $ drop x row
  | otherwise = reverse $ take actuallyTake $ drop (x+n+1) row
  where row = ws !! y
        actuallyTake = min (-n) (x+1)

vtake :: Grid a -> Coord -> Int -> [a]
vtake _ _ 0 = []
vtake ws c 1 = [ws !.! c]
vtake ws c (-1) = [ws !.! c]
vtake ws (x, y) n
  | n > 0 = take n $ drop y col
  | otherwise = reverse $ take actuallyTake $ drop (y+n+1) col
  where col = ws !|! x
        actuallyTake = min (-n) (y+1)

ldtake :: Grid a -> Coord -> Int -> [a]
ldtake _ _ 0 = []
ldtake ws c 1 = [ws !.! c]
ldtake ws c (-1) = [ws !.! c]
ldtake ws c n
  | n > 0 = reverse $ take actuallyTake $ drop (i-n+1) diag
  | otherwise = take (-n) $ drop i diag
  where diag = ws !/! c
        i = indexld ws c
        actuallyTake = min n (i+1)

rdtake :: Grid a -> Coord -> Int -> [a]
rdtake _ _ 0 = []
rdtake ws c 1 = [ws !.! c]
rdtake ws c (-1) = [ws !.! c]
rdtake ws c n
  | n > 0 = take n $ drop i diag
  | otherwise = reverse $ take actuallyTake $ drop (i+n+1) diag
  where diag = ws !\! c
        i = indexrd ws c
        actuallyTake = min (-n) (i+1)

set :: Grid a -> Coord -> a -> Grid a
set old (x,y) item
  | (x < width old && x >= 0) && (y < height old && y >= 0) = take y old ++ row' : drop (y+1) old
  | otherwise = old
  where row = old !! y
        row' = take x row ++ item : drop (x+1) row
