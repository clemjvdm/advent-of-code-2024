-- mul\([0-9]+,[0-9]+\)
import Text.Regex.TDFA ((=~))
import Text.Regex (matchRegex)

main = do 
  contents <- readFile "input.txt"
  let reg = contents =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]
  print $ calc reg

calc :: [[String]] -> Int
calc xs = sum [ (read x)*(read y) | [_,x,y] <- xs ]
