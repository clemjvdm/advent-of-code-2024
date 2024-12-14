import Text.Regex.TDFA ((=~))

main = do
  contents <- readFile "input.txt"
  let reg = contents =~ "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)" :: [[String]]
  print (calcDo reg)

calcDo :: [[String]] -> Int
calcDo [] = 0
calcDo xs = sum [ if d == "do()" then 0 else read x*read y | [d, x, y] <- dos] + calcDont (drop (length dos +1) xs)
  where dos = takeWhile (\[d,_,_] -> d /= "don't()") xs

calcDont :: [[String]] -> Int
calcDont [] = 0
calcDont xs = calcDo (drop (length donts +1) xs)
  where donts = takeWhile (\[d,_,_] -> d /= "do()") xs
