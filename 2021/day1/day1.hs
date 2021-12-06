module Day1 where

-- Recursive
getIncCount :: [Int] -> Int
getIncCount [] = 0
getIncCount [_] = 0
getIncCount (x:x':xs)
  | x' > x    = 1 + getIncCount (x':xs)
  | otherwise = getIncCount (x':xs)

-- List comprehension
getIncCount' [] = 0
getIncCount' [_] = 0
getIncCount' xs = length [y | (x, y) <- zip xs $ tail xs, y > x]

-- sum over zipWith
getIncCount'' [] = 0
getIncCount'' [_] = 0
getIncCount'' xs = sum $ zipWith (\x y -> if (y>x) then 1 else 0) xs $ tail xs

main :: IO ()
main = do
  content <- readFile "input.txt"
  print $ getIncCount (map read (lines content) :: [Int])

