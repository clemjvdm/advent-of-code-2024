import Data.List (sort)
import System.IO
import Data.List.Split

main = do 
  contents <- lines <$> readFile "input.txt"
  let (a, b) = unzip [(read a, read b) | line <- contents, let [a,b] = words line]
  print (totalDist a b)

totalDist :: [Int] -> [Int] -> Int
totalDist xs ys = sum [ abs (x-y) | (x, y) <- zip (sort xs) (sort ys) ]
