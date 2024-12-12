import Data.List (sort)
import System.IO

main = do
  contents <- lines <$> readFile "input.txt"
  let levels :: [[Int]] = map read . words <$> contents
  let safe = length $ filter safeCheck levels
  print safe 

safeCheck :: [Int] -> Bool
safeCheck xs = orderCheck && diffCheck
  where
    diffList = zipWith (\x y -> abs (x-y)) xs (tail xs)
    diffCheck = (maximum diffList <= 3) && (minimum diffList >= 1)
    orderCheck = sort xs == xs || sort xs == reverse xs
