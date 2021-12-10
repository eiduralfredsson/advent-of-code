module Day4 where

import Data.List (intersect,transpose,sort,(\\))
import qualified Data.Text as T

type Board = [[Int]]
type RowCol = [Int] -- A row or column
type Draw = [Int]

rows :: Board -> Board
rows b = b

cols :: Board -> Board
cols b = transpose b

-- We can add the transposed columns at the bottom for easier processing
rowsAndCols :: Board -> Board
rowsAndCols b = rows b ++ cols b

-- Is a given row or column complete?
winningRowCol :: Draw -> RowCol -> Bool
winningRowCol d rc = intersect sd src == src
  where sd  = sort d
        src = sort rc

-- Does a given board have a complete row or column?
completeBoard :: Draw -> Board -> Bool
completeBoard d b = completeBoard' d $ rowsAndCols b
completeBoard' _ [] = False
completeBoard' d (rc:rcs)
  | winningRowCol d rc = True
  | otherwise          = completeBoard' d rcs

-- Does a list of boards contain a complete board?
winningBoard :: Draw -> [Board] -> Maybe Board
winningBoard _ [] = Nothing
winningBoard d (b:bs)
  | completeBoard d b = Just b
  | otherwise       = winningBoard d bs

-- Find the unmarked numbers on a board
unmarked :: Draw -> Board -> [Int]
unmarked d b = concat b \\ d

-- Play the game (draw until a board wins or we run out of numbers)
-- First param is the previously drawn numbers
-- Second param is the remaining numbers to draw
draw :: Draw -> Draw -> [Board] -> Maybe Int
draw _ [] _ = Nothing
draw prevDraw (d:ds) bs = case winningBoard currDraw bs of
  Nothing -> draw currDraw ds bs
  Just b -> Just (d * (sum $ unmarked currDraw b))
  where currDraw = d : prevDraw

-- Create the draw from inputs
mkDraw :: T.Text -> Draw
mkDraw t = map (read . T.unpack) $ T.splitOn (T.pack ",") $ t :: Draw

-- Create a board list from inputs
mkBoards :: [T.Text] -> [Board]
mkBoards [] = []
mkBoards (b:bs) = mkBoard b : mkBoards bs
  where mkBoard x = (map . map) (read . T.unpack) $ map T.words $ T.splitOn (T.pack "\n") $ T.strip x :: Board

report :: String -> IO ()
report path = do
  content <- readFile path
  let blocks = T.splitOn (T.pack "\n\n") (T.pack content) -- The draw in row 1 and boards below
  let d  = mkDraw $ head blocks
  let bs = mkBoards $ tail blocks
  let score = draw [] d bs
  print score

