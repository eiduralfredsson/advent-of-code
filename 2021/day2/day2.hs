module Day2 where

type HPos = Int
type Depth = Int
type Coords = (HPos,Depth)
type Instr = (String,Int) -- e.g. ("forward",5)

forward :: Int -> Coords -> Coords
forward n (h,d) = (h+n,d)

down :: Int -> Coords -> Coords
down n (h,d) = (h,d+n)

up :: Int ->  Coords -> Coords
up n (h,d) = (h,d-n)

move :: Instr -> Coords -> Coords
move ("forward",n) coords = forward n coords
move ("down",n) coords = down n coords
move ("up",n) coords = up n coords

batchMove :: [Instr] -> Coords -> Coords
batchMove [] coords = coords
batchMove (i:is) coords = batchMove is $ move i coords

multCoords :: Coords -> Int
multCoords (h,d) = h*d

-- transform a line like "forward 5" to ("forward",5) - yuk!
lineToInstr :: String -> Instr
lineToInstr line = (wordList !! 0, read $ wordList !! 1 :: Int)
  where wordList = words line

report :: String -> IO ()
report path = do
  content <- readFile path
  let instr = map lineToInstr $ lines content
  print $ multCoords $ batchMove instr (0,0)

