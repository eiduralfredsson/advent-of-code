module Day2Part2 where

{-
The part 1 solution seems to be a good basis for solving part 2.
We should only have to add Aim to the coordinates and change forward/down/up.
-}

type HPos = Int
type Depth = Int
type Aim = Int -- This is new
type Coords = (HPos,Depth,Aim) -- This has changed
type Instr = (String,Int) -- e.g. ("forward",5)

forward :: Int -> Coords -> Coords
forward n (h,d,a) = (h+n,d+a*n,a) -- This has changed

down :: Int -> Coords -> Coords
down n (h,d,a) = (h,d,a+n) -- This has changed

up :: Int ->  Coords -> Coords
up n (h,d,a) = (h,d,a-n) -- This has changed

move :: Instr -> Coords -> Coords
move ("forward",n) coords = forward n coords
move ("down",n) coords = down n coords
move ("up",n) coords = up n coords

batchMove :: [Instr] -> Coords -> Coords
batchMove [] coords = coords
batchMove (i:is) coords = batchMove is $ move i coords

multCoords :: Coords -> Int
multCoords (h,d,_) = h*d -- This has changed

-- transform a line like "forward 5" to ("forward",5) - yuk!
lineToInstr :: String -> Instr
lineToInstr line = (wordList !! 0, read $ wordList !! 1 :: Int)
  where wordList = words line

report :: String -> IO ()
report path = do
  content <- readFile path
  let instr = map lineToInstr $ lines content
  print $ multCoords $ batchMove instr (0,0,0)

