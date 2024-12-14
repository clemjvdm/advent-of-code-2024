import Data.List (sort)
import System.IO

main = do
  contents <- lines <$> readFile "input.txt"
  let levels :: [[Int]] = map read . words <$> contents
  let safe = length $ filter (\level -> safeCheckR level [] || safeCheckR (reverse level) []) levels
  print safe

safeCheckR :: [Int] -> [Int] -> Bool
safeCheckR [x,y] _ = True
safeCheckR (x:y:xs) ws
  | inBounds (y-x) = safeCheckR (y:xs) (ws ++ [x])
  | otherwise = safeCheck (ws ++ (y:xs)) || safeCheck (ws ++ x:xs)

safeCheck :: [Int] -> Bool
safeCheck [x,y] = x < y && (y-x) >= 1 && (y-x) <= 3
safeCheck (x:y:xs) = x < y && (y-x) >= 1 && (y-x) <= 3 && safeCheck (y:xs)

inBounds :: Int -> Bool
inBounds x = x >= 1 && x <= 3
