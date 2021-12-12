module Day9 where

import Data.Char
import Data.List

-- The idea is:
-- We'll find lows for left and right using the original list.
-- We'll find lows for up and down using the transposed list.
-- We'll join the two together for the final result.
-- We can apply the same logic within a row, where we find a
-- low to the left using the original row, and a low to the right
-- using the reversed row.

-- Find out whether the readings in a given row are lows to their left.
-- The leftmost value is always a low to its left, hence the first True.
leftLow :: [Int] -> [Bool]
leftLow xs = True : [ y < x | (x,y) <- zip xs $ tail xs ]

-- Combine left and right lows
bothLow :: [Bool] -> [Bool] -> [Bool]
bothLow xs ys = [ x && y | (x,y) <- zip xs ys ]

-- Process all the rows in a map
mapLow :: [[Int]] -> [[Bool]]
mapLow [] = []
mapLow (xs:xss) = bothLow (leftLow xs) (reverse . leftLow $ reverse xs) : mapLow xss

-- Combine lows from rows and columns
allLow :: [[Bool]] -> [[Bool]] -> [[Bool]]
allLow [] [] = []
allLow (xs:xss) (ys:yss) = bothLow xs ys : allLow xss yss

-- Process the entire map
processMap :: [[Int]] -> [[Bool]]
processMap xss = allLow (mapLow xss) (transpose $ mapLow $ transpose xss)

-- Calculate risk level by comparing the original map and the corresponding truth table
riskLevel :: [[Int]] -> [[Bool]] -> Int
riskLevel [] [] = 0
riskLevel (xs:xss) (ys:yss) = sum [ x + 1 | (x,y) <- zip xs ys, y ] + riskLevel xss yss

report :: String -> IO ()
report path = do
  content <- readFile path
  let hm = (map . map) digitToInt $ lines content
  let pm = processMap hm
  let rl = riskLevel hm pm
  print rl
