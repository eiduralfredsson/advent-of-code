{-# LANGUAGE OverloadedStrings #-}

module Day6Part2 where

import qualified Data.Text as T

-- Part 1 doesn't perform, so for part 2 we'll just use a simple 9 element list
-- where each index represents an age and the element represents the number of fish
type Population = [Integer]

-- Each tick of the day clock shifts the numbers down the list (and wraps around).
-- Additionally, day six gets the number for day zero added
dong :: Integer -> Population -> Population
dong 0 xs = xs
dong n (zero:one:two:three:four:five:six:seven:eight:xs)
  = dong (n-1) $ one:two:three:four:five:six:seven+zero:eight:zero:xs

-- Make a correctly-formatted initial list from file input
mkInit :: [Int] -> Population
mkInit xs = mkInit' xs [0,0,0,0,0,0,0,0,0]
mkInit' [] ys = ys
mkInit' (x:xs) ys = mkInit' xs $ take x ys ++ [(ys !! x) + 1] ++ drop (x+1) ys

report :: String -> IO ()
report path = do
  content <- readFile path
  let initial = map (read . T.unpack) $ T.splitOn "," $ T.strip $ T.pack content :: [Int]
  let initPop = mkInit initial
  let finalPop = dong 256 initPop
  let res = sum finalPop
  print initPop
  print finalPop
  print res
