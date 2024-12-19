import Data.List

main = do
  contents <- lines <$> readFile "input.txt"
  let result = solve "XMAS" contents + solve (reverse "XMAS") contents
  print result

solve :: String -> [String] -> Int
solve w l = rows + cols + diar + diar' + dial + dial' - ider - idel
  where
    rows = sum $ map (countWord w) l
    cols = sum $ map (countWord w) $ transpose l
    diar = sum $ map (countWord w) $ transpose $ mapIndex (flip drop) l
    diar' = sum $ map (countWord w) $ transpose $ mapIndex (flip drop) $ transpose l
    dial = sum $ map (countWord w) $ transpose $ mapIndex (flip drop) $ reverse l
    dial' = sum $ map (countWord w) $ transpose $ mapIndex (flip drop) $ transpose $ reverse l
    ider = countWord w $ head $ transpose $ mapIndex (flip drop) l
    idel = countWord w $ head $ transpose $ mapIndex (flip drop) $ reverse l

countWord :: String -> String -> Int
countWord w l = sum [ 1 | i <- tails l, take (length w) i == w]

mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f l = zipWith f l [0..length l - 1]
