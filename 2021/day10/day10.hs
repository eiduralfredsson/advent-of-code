module Day10 where

closeFor :: Char -> Char -> Bool
closeFor ')' '(' = True
closeFor ']' '[' = True
closeFor '}' '{' = True
closeFor '>' '<' = True
closeFor _ _     = False
  
processChar :: Char -> String -> Maybe String
processChar x ys
  | x `elem` "([{<" = Just $ x:ys
  | x `elem` ")]}>" = if  x `closeFor` head ys then Just $ drop 1 ys else Nothing
  | otherwise       = Nothing

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points _   = 0

processLine :: String -> String -> Int
processLine _ ""  = 0
processLine xs (y:ys) = case processChar y xs of
  Nothing -> points y
  Just zs -> processLine zs ys

processAll :: [String] -> Int
processAll xss = sum $ map (processLine "") xss

report :: String -> IO ()
report path = do
  contents <- readFile path
  let ls = lines contents
  let res = processAll ls
  print res
