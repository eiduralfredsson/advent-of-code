module Day10Part2 where

import Data.List

type Bracket = Char 
type Stack = [Bracket]
type Line = [Bracket]
data Score = Success Int | Corrupt Int | Incomplete Int
  deriving Show

getScore :: Score -> Int
getScore (Success n)    = n
getScore (Corrupt n)    = n
getScore (Incomplete n) = n

-- Push the next bracket onto the stack
push :: Bracket -> Stack -> Stack
push = (:)

-- Pop a bracket off the stack
pop :: Stack -> Stack
pop = drop 1

-- Is a character an opening bracket?
opening :: Bracket -> Bool
opening b = b `elem` "([{<"

-- Get the matching closing bracket
closing :: Bracket -> Bracket
closing '(' = ')'
closing '[' = ']'
closing '{' = '}'
closing '<' = '>'

-- Push or pop as appropriate.
-- This fails if a closing bracket doesn't match the opening bracket on the stack.
op :: Bracket -> Stack -> Either Stack Stack
op b s
  | opening b   = Right $ push b s -- Opening brackets always go on the stack
  | b == (closing $ head s) = Right $ pop s    -- Closing brackets pop the matching opening brackets off the stack
  | otherwise   = Left s           -- Closing brackets don't match the next bracket on the stack

opLine :: Line -> Score
opLine l = opLine' l ""
opLine' :: Line -> Stack -> Score
-- All line items have made it on to the stack and back off again (not happening in this puzzle)
opLine' "" "" = Success 0
-- All line items have made it on to the stack but some are left on the stack (part 2 problem)
opLine' "" s  = scoreIncomplete s
-- Some line items left - keep processing
opLine' (b:bs) s = case op b s of
  Right s' -> opLine' bs s' -- Success - go around again
  Left s'  -> scoreCorrupt b -- Failure (part 1 problem)

-- Calculate score for incomplete stack
scoreIncomplete :: Stack -> Score
scoreIncomplete s = Incomplete $ foldl (\b a -> 5 * b + a) 0 $ map (sc . closing)  s
  where
    sc ')' = 1
    sc ']' = 2
    sc '}' = 3
    sc '>' = 4

-- Calculate score for incorrect closing bracket
scoreCorrupt :: Bracket -> Score
scoreCorrupt ')' = Corrupt 3
scoreCorrupt ']' = Corrupt 57
scoreCorrupt '}' = Corrupt 1197
scoreCorrupt '>' = Corrupt 25137

scoreAll :: [Line] -> [Score]
scoreAll = map opLine

-- Return separate scores for corrupt and incomplete
totalCorruptScore :: [Score] -> Int
totalCorruptScore [] = 0
totalCorruptScore (Corrupt n:ss) = n + totalCorruptScore ss
totalCorruptScore (_:ss) = totalCorruptScore ss

totalIncompleteScore :: [Score] -> [Int]
totalIncompleteScore [] = []
totalIncompleteScore (Incomplete n:ss) = n : totalIncompleteScore ss
totalIncompleteScore (_:ss) = totalIncompleteScore ss

middleIncompleteScore :: [Int] -> Int
middleIncompleteScore xs = (sort xs) !! (length xs `div` 2)


report :: String -> IO ()
report path = do
  contents <- readFile path
  let ls = lines contents
  let sa = scoreAll ls
  let tc = totalCorruptScore sa
  let ti = totalIncompleteScore sa
  let mi = middleIncompleteScore ti
  putStr "Total corrupt: "
  print tc
  putStr "Total incomplete: "
  print ti
  putStr "Middle incomplete: "
  print mi

