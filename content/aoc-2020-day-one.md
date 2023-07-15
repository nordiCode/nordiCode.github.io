+++
title = "AOC 2022"
date = 2022-07-15
[taxonomies]
tags = ["AOC","haskell"]

+++

In order to deepen my knowledge of Haskell, I have decided to challenge myself by attempting Advent of Code problems. Although my solutions may not yet be idiomatic, I am confident that they will improve as I continue to learn and gain more experience.

### 2022 Day One


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

### 2022 Day Two

```haskell
data Piece = Rock | Paper | Scissors
  deriving (Show, Eq, Enum)

outCome :: (Piece, Piece) -> Int
outCome (x, y)
  | y == Rock && x == Scissors || y == Paper && x == Rock || y == Scissors && x == Paper = 6
  | x == y = 3
  | otherwise = 0

changeSecond :: (Piece, Piece) -> (Piece, Piece)
changeSecond (m, n)
  | n == Scissors = swapWin (m, n)
  | n == Paper = swapDraw (m, n)
  | n == Rock = swapLose (m, n)

swapWin :: (Piece, Piece) -> (Piece, Piece)
swapWin (m, _)
  | m == Rock = (m, Paper)
  | m == Paper = (m, Scissors)
  | otherwise = (m, Rock)

swapDraw :: (Piece, Piece) -> (Piece, Piece)
swapDraw (m, _) = (m, m)

swapLose :: (Piece, Piece) -> (Piece, Piece)
swapLose (m, _)
  | m == Rock = (m, Scissors)
  | m == Paper = (m, Rock)
  | otherwise = (m, Paper)

sumResult :: (Piece, Piece) -> Int
sumResult (x, y) = fromEnum y + 1 + outCome (x, y)

-- need to not use Head/Tail
mapTuple :: [b] -> (b, b)
mapTuple x = (head x, last x)

tuplePiece :: (Char, Char) -> (Piece, Piece)
tuplePiece (m, n) = (mapPiece m, mapPiece n)

mapPiece :: Char -> Piece
mapPiece x
  | x == 'A' || x == 'X' = Rock
  | x == 'B' || x == 'Y' = Paper
  | x == 'C' || x == 'Z' = Scissors

main :: IO ()
main = do
  contents <- readFile "day_two.txt"
  -- print $ sum . map sumResult $ map changeSecond $ map mapTuple $ lines contents
  print $ sum . map (sumResult . tuplePiece . mapTuple) $ lines contents
```

[Advent of Code](https://adventofcode.com/)