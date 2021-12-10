{-# LANGUAGE OverloadedStrings #-}

module Day5Part2 where

import Data.List
import qualified Data.Text as T

type Coord = (Int,Int)

-- Make a list of all coordinates on horizontal or vertical lines between two coordinates
coords :: (Coord,Coord) -> [Coord]
coords ((x,y),(x',y'))
  | x == x' || y == y' = nub $ horizontal ++ vertical
  | otherwise          = diagonal                                  -- NEW for part 2
  where horizontal  = [(x'',y) | x'' <- [(min x x')..(max x x')]]
        vertical    = [(x,y'') | y'' <- [(min y y')..(max y y')]]
        diagonal    = zip (diag (x,x')) (diag (y,y'))              -- NEW for part 2
        diag (n,n') = if n < n' then [n..n'] else reverse [n'..n]  -- NEW for part 2

-- Get number of coordinates that show up n times or more
frequent :: (Eq a, Ord a) => Int -> [a] -> Int
frequent n = length . filter (\x -> length x >= n) . group . sort -- composition, baby!

-- Build a list of coordinate pairs
mkCoords :: String -> [(Coord,Coord)]
mkCoords s = toOuterTup $ (map . map) toInnerTup $ removeComma $ removeArrow $ removeNewline (T.strip $ T.pack s)
  where removeNewline = T.splitOn "\n"
        removeArrow   = map (T.splitOn " -> ")
        removeComma   = (map . map) (T.splitOn ",")
        toInnerTup xs = ((read . T.unpack) $ head xs, (read . T.unpack) $ last xs) :: Coord
        toOuterTup    = map (\xs -> (head xs, last xs))

-- Make the full list of coordinates from a list of coordinate pairs
allCoords :: [(Coord,Coord)] -> [Coord]
allCoords cs = concat [coords c | c <- cs]

report :: String -> IO ()
report path = do
  content <- readFile path
  let c = mkCoords content
  let ac = allCoords c
  let f = frequent 2 ac
  print f
