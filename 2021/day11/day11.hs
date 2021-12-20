module Day11 where

import           Data.Char
import qualified Data.Map  as Map

type Row = Int
type Col = Int
type Coord = (Row,Col)
type Energy = Int
type Flashed = Bool
type Oct = (Energy,Flashed)
type Grid = Map.Map Coord Oct

initGrid :: String -> Grid
initGrid xss = Map.fromList $ zip [(x,y) | x <- [0..9], y <- [0..9]] $ [(x,False) | x <- concat $ (map . map) digitToInt $ lines xss]

stepGrid :: Grid -> Grid
stepGrid g = Map.map (\(e,b) -> (e+1,b)) g

-- Find neighbours
findNb :: Coord -> [Coord]
findNb (r,c) = [(x,y) | x <- [r-1..r+1], y <- [c-1..c+1], (x,y) /= (r,c), x >= 0, y >= 0, x <= 9, y <= 9]

process :: String -> IO ()
process path = do
  contents <- readFile path
  let grid = initGrid contents
  print grid
  print $ Map.lookup (1,1) grid
  print $ stepGrid grid

