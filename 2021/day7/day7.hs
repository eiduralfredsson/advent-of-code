{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import qualified Data.Text as T

type Crab = Int
type Target = Int
type Fuel = Int

-- Get a list of all possible targets
targets :: [Crab] -> [Target]
targets cs = [minimum cs .. maximum cs]

-- Calculate fuel cost of moving all crabs to given target
fuel :: [Crab] -> Target -> Fuel
fuel cs t = sum $ map (\x -> abs $ t - x) cs

-- Get a list of all possible fuel costs
allFuel :: [Crab] -> [Target] -> [Fuel]
allFuel cs = map (fuel cs)

-- Get optimal fuel number
minFuel :: [Fuel] -> Fuel
minFuel = minimum


report :: String -> IO ()
report path = do
  content <- readFile path
  let cs = map (read . T.unpack) $ T.splitOn "," $ T.strip $ T.pack content :: [Crab]
  let ts = targets cs
  let fs = allFuel cs ts
  let res = minFuel fs
  print res
