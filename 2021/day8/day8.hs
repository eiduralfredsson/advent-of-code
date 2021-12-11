module Day8 where

import Data.List
import Data.Maybe

type Segment = Int
type Digit = Int
type Display = [Segment]
type Entry = [String]

-- We'll operate on integers where we can
charToSeg :: Char -> Segment
charToSeg c = case elemIndex c ['a'..'g'] of
  Just n  -> n
  Nothing -> (-1)

-- Some digits can be found based on the number of active segments alone
getEncrDigit :: Display -> Maybe Digit
getEncrDigit d = case length d of
  2 -> Just 1
  4 -> Just 4
  3 -> Just 7
  7 -> Just 8
  _ -> Nothing

-- Get a list of output display digits from a single entry
-- (the bits after the "|" in integer form)
getOutput :: [Entry] -> [Maybe Digit]
getOutput [] = []
getOutput (e:es) = (map getEncrDigit $ (map . map) charToSeg $ drop 11 e) ++ getOutput es

process :: String -> IO ()
process path = do
  content <- readFile path
  let entries = map words . lines $ content
  let out = getOutput entries
  let res = length . filter isJust $ out
  print res
-- (in,out)
