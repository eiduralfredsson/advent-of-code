module Day3Part2 where

import Data.Char (digitToInt)

-- We'll represent a binary digit thus
data Bin = O | I deriving (Eq, Show)

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
type O2GenRating = BinString
type CO2ScrRating = BinString
type LifeSuppRating = Int

-- Transform a binary number to an integer
binStrToInt :: BinString -> Int
binStrToInt [] = 0
binStrToInt bs@(b:bs') = 2 ^ (length bs - 1) * fromEnum b + binStrToInt bs'

-- Find the most common bit digit in a given column.
mostCommonDigit :: BinString -> Bin
mostCommonDigit bs
  | (length $ filter (==I) bs) >= (len `div` 2 + len `mod` 2) = I
  | otherwise                                                 = O
  where len = length bs

-- Lest common digit is the opposite of the most common.
leastCommonDigit :: BinString -> Bin
leastCommonDigit bs = (succ . mostCommonDigit) bs

-- Gamma rate calculation
gammaRate :: [BinString] -> GammaRate
gammaRate ([]:_) = []
gammaRate bss = (mostCommonDigit $ map head bss) : (gammaRate $ map (drop 1) bss)
                
-- We can just flip the gamma rate bits
gammaToEpsilon :: BinString -> EpsilonRate
gammaToEpsilon = map succ

-- Power consumption
powerCons :: GammaRate -> EpsilonRate -> PowerCons
powerCons g e = binStrToInt g * binStrToInt e

-- Transform a string of 0s and 1s into a binary string
strToBinStr :: String -> BinString
strToBinStr xs = map (toEnum . digitToInt) xs

-- Oxygen generator rating (this and below is hideous! - need to do better)
o2GenRate :: [BinString] -> O2GenRating
o2GenRate bss = o2co2Rate bss 0 mostCommonDigit

-- CO2 scrubber rating
co2ScrRate :: [BinString] -> CO2ScrRating
co2ScrRate bss = o2co2Rate bss 0 leastCommonDigit

-- Helper for both o2 and co2 calcs
o2co2Rate :: [BinString] -> Int -> (BinString -> Bin) -> O2GenRating
o2co2Rate [] _ _   = []
o2co2Rate [bs] _ _ = bs
o2co2Rate bss i f  = o2co2Rate (filter (\bs -> bs !! i == f (map (!! i) bss)) bss) (i+1) f

-- Life support rating
lifeSupport :: O2GenRating -> CO2ScrRating -> LifeSuppRating
lifeSupport o2 co2 = binStrToInt o2 * binStrToInt co2

report :: String -> IO ()
report path = do
  content <- readFile path
  let diag = map strToBinStr $ lines content
  let gamma = gammaRate diag
  let epsilon = gammaToEpsilon gamma
  let pc = powerCons gamma epsilon
  let o2 = o2GenRate diag
  let co2 = co2ScrRate diag
  let ls = lifeSupport o2 co2
  print gamma
  print epsilon
  print pc
  print o2
  print co2
  print ls

