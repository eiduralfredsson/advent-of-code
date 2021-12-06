module Day1Part2 where

-- Recursive
getIncCount :: [Int] -> Int
getIncCount [] = 0
getIncCount [_] = 0
getIncCount (x:x':xs)
  | x' > x    = 1 + getIncCount (x':xs)
  | otherwise = getIncCount (x':xs)

-- List comprehension
getIncCount' :: [Int] -> Int
getIncCount' [] = 0
getIncCount' [_] = 0
getIncCount' xs = length [y | (x, y) <- zip xs $ tail xs, y > x]

-- sum over zipWith
getIncCount'' :: [Int] -> Int
getIncCount'' [] = 0
getIncCount'' [_] = 0
getIncCount'' xs = sum $ zipWith (\x y -> if (y>x) then 1 else 0) xs $ tail xs

-- build a three-measurement sliding window
mkWindow :: [Int] -> [(Int,Int,Int)]
mkWindow xs@(x:x':x'':x''') = (x,x',x'') : (mkWindow . tail) xs
mkWindow _             = []

-- sum the triples in a sliding window
sumWindow :: [(Int,Int,Int)] -> [Int]
sumWindow xs = map (\(x,y,z) -> x+y+z) xs

-- Output action. Takes the file path and the counting funtion (one of the getIncCount* above).
-- Example: report "input.txt" getIncCount''
report :: String -> ([Int] -> Int) -> IO ()
report path f = do
  content <- readFile path
  let measurements = map read $ lines content :: [Int]
  putStr "Part 1 answer:"
  print $ f measurements
  putStr "Part 2 answer:"
  print $ f . sumWindow . mkWindow $ measurements



