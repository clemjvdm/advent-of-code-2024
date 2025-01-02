import Data.Map (Map)
import Data.List.Split (splitOn)
import Data.Map.Lazy (insertWith, empty, (!), member)


main = do
  contents <- readFile "input.txt"
  let ordering :: [(Int, Int)] = [ (read $ head $ splitOn "|" x, read $ last $ splitOn "|" x) | x <- lines $ head $ splitOn "\n\n" contents]
  let prec = buildPrec empty ordering
  let updates = map (map read . splitOn ",") $ lines $ last $ splitOn "\n\n" contents
  let s = sum [ if valid prec [] x then middle x else 0 | x <- updates ]
  print s

buildPrec :: Map Int [Int] -> [(Int, Int)] -> Map Int [Int]
buildPrec map [] = map
buildPrec m ((l,r):xs) = buildPrec (insertWith (++) l [r] m) xs

valid :: Map Int [Int] -> [Int] -> [Int] -> Bool
valid m xs [] = True
valid m xs (y:ys) = 
  if member y m then (sum [ if i `elem` xs then 1 else 0 | i <- m ! y] == 0) && valid m (xs ++ [y]) ys else valid m (xs ++ [y]) ys

middle :: [Int] -> Int
middle [] = 0
middle xs = f xs xs
  where f (x:_) [_] = x
        f (_:xs) (_:_:ys) = f xs ys
