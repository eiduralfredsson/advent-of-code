module Day3 where

import Data.Char (digitToInt)

-- We'll represent a binary digit thus
data Bin = O | I deriving Show

-- Enum gives us a way to transform to Int and flip bits
instance Enum Bin where
  toEnum n = case n `mod` 2 of
    0 -> O -- even to O
    1 -> I -- odd to I
  fromEnum O = 0
  fromEnum I = 1
  succ O = I -- bit flip; works for pred too
  succ I = O -- bit flip; works for pred too

type BinString = [Bin]
type Column = BinString
type GammaRate = BinString
type EpsilonRate = BinString
type PowerCons = Int

-- Transform a binary number to an integer
binStrToInt :: BinString -> Int
binStrToInt [] = 0
binStrToInt bs@(b:bs') = 2 ^ (length bs - 1) * fromEnum b + binStrToInt bs'

-- Find the most common bit digit in a given column.
-- We compare the height of the column to the sum of the digits. If the sum is half or
-- more of the height, the most common digit it I, otherwise O.
mostCommon :: Column -> Bin
mostCommon bs = (\x -> if (x >= length bs `div` 2) then I else O) $ sum $ map fromEnum bs

-- Gamma rate calculation
gammaRate :: [BinString] -> GammaRate
gammaRate ([]:_) = []
gammaRate bss = (mostCommon $ head bss) : (gammaRate $ map (drop 1) bss)
                
-- We can just flip the gamma rate bits
gammaToEpsilon :: BinString -> EpsilonRate
gammaToEpsilon = map succ

-- Power consumption
powerCons :: GammaRate -> EpsilonRate -> PowerCons
powerCons g e = binStrToInt g * binStrToInt e

-- Transform a string of 0s and 1s into a binary string
strToBinStr :: String -> BinString
strToBinStr xs = map (toEnum . digitToInt) xs

report :: String -> IO ()
report path = do
  content <- readFile path
  let diag = map strToBinStr $ lines content
  let gamma = gammaRate diag
  let epsilon = gammaToEpsilon gamma
  let pc = powerCons gamma epsilon
  print gamma
  print epsilon
  print pc
