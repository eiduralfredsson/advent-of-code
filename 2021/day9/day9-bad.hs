module Day9 where

import Data.Char

-- The idea is:
-- Once we know the width of the map (the line length), we turn the map into a single
-- string and move a sliding measurement window over it. The measuring window reads the
-- current height and the up/down/left/right heights. We need to be careful when we're
-- close to the edges of the string.

type HeightMap = [MapLine]  -- The map as it's given to us
type MapLine = String       -- A single line in the original map
type Width = Int            -- The width of the map
type FlattenedMap = String  -- The map as a single string
type Height = Char          -- A reading from the map
type Low = Char             -- A low
type Pos = Int              -- The position of the sliding window

-- Flatten the map
flatten :: HeightMap -> FlattenedMap
flatten m = concat m

-- Given a flattened map, find all the lows
lows :: FlattenedMap -> Width -> [Low]
lows xs w = lows' xs w 0 []
lows' xs w p ls
  | p == length xs - 1 = ls
  | isLow              = lows' xs w (p+1) $ currHeight : ls
  | otherwise          = lows' xs w (p+1) ls
  where
    isLow = lessThanUp xs w p &&
            lessThanDown xs w p &&
            lessThanLeft xs p &&
            lessThanRight xs p
    currHeight = xs !! p

lessThanUp :: FlattenedMap -> Width -> Pos -> Bool
lessThanUp xs w p
  | p < w     = True
  | otherwise = xs !! p  < xs !! (p - w)

lessThanDown :: FlattenedMap -> Width -> Pos -> Bool
lessThanDown xs w p
  | p > length xs - w = True
  | otherwise         = xs !! p < xs !! (p + w)

lessThanLeft :: FlattenedMap -> Pos -> Bool
lessThanLeft xs p
  | p < 1     = True
  | otherwise = xs !! p < xs !! (p - 1)

lessThanRight :: FlattenedMap -> Pos -> Bool
lessThanRight xs p
  | p > length xs - 1 = True
  | otherwise         = xs !! p < xs !! (p + 1)

-- Calculate risk level
--riskLevel :: [Low] -> Int
riskLevel = sum . map ((+1) . digitToInt)

report :: String -> IO ()
report path = do
  content <- readFile path
  let l = lines content
  let w = (length . head) l
  let f = flatten l
  let ls = lows f w
  let rl = riskLevel ls
  print w
  print f
  print ls
  print rl
