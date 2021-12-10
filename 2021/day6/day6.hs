{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import qualified Data.Text as T

type Fish = Int

-- Age a single fish by a day, sometimes producing a baby
tick :: Fish -> (Fish, Maybe Fish)
tick f = case f of
  0 -> (6, Just 8)
  n -> (n-1, Nothing)

-- Age the entire population by a day
tock :: [Fish] -> [Fish]
tock [] = []
tock (x:xs) = case tick x of
  (f, Nothing) -> f : tock xs
  (f, Just f') -> f' : f : tock xs

-- Age the entire population for n days
ding :: Int -> [Fish] -> Int
ding 0 xs = length xs
ding n xs = ding (n-1) $ tock xs

report :: String -> IO ()
report path = do
  content <- readFile path
  let initial = map (read . T.unpack) $ T.splitOn "," $ T.strip $ T.pack content :: [Fish]
  print $ ding 80 initial
