module Day1 where

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

report :: String -> ([Int] -> Int) -> IO ()
report path f = do
  content <- readFile path
  let measurements = map read $ lines content :: [Int]
  print $ f measurements

