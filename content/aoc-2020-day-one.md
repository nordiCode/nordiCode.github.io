+++
title = "AOC 2022 Day One"
date = 2022-07-15
[taxonomies]
tags = ["AOC","haskell"]

+++


```haskell
import Data.List (groupBy, sort)
import System.IO

partOne :: [[String]] -> Int
partOne = maximum . map sum . intArrays

partTwo :: [[String]] -> Int
partTwo = sum . take 3 . reverse . sort . map sum . intArrays

cleanContents :: String -> [[[Char]]]
cleanContents = filter (not . null) . map (filter (/= "")) . groupBy (\x y -> x /= "" && y /= "") . lines

intArrays :: [[String]] -> [[Int]]
intArrays = map readInt

readInt :: [String] -> [Int]
readInt = map read

main :: IO ()
main = do
  contents <- readFile "day_one_input.txt"
  let groups = cleanContents $ contents
  let maxCal = partOne groups
  let maxCalTopThree = partTwo groups
  print $ "Solution Part One " <> show maxCal
  print $ "Solution Part Two " <> show maxCalTopThree
```

<!-- more -->

The write up