import Data.List (sort)
import System.IO

main = do
  contents <- lines <$> readFile "input.txt"
  let levels :: [[Int]] = map read . words <$> contents
  let safe = length $ filter (\level -> safeCheck level || safeCheck (reverse level)) levels
  print safe 

safeCheck :: [Int] -> Bool
safeCheck [x,y] = x < y && (y-x) >= 1 && (y-x) <= 3
safeCheck (x:y:xs) = x < y && (y-x) >= 1 && (y-x) <= 3 && safeCheck (y:xs)
