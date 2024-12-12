import Data.List (sort)
import System.IO
import Data.List.Split

main = do
  contents <- lines <$> readFile "input.txt"
  let (a, b) = unzip [(read a, read b) | line <- contents, let [a,b] = words line]
  print (similarityScore a b)

similarityScore :: [Int] -> [Int] -> Int
similarityScore a b = sum [ length (filter (==elem) b) * elem | elem <- a ]