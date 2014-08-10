main :: IO ()
main = print . fst . head . dropWhile ((< 500) . snd) $ map factors $ filter (>1000000) triangleNumbers
  where factors n = (n, length $ filter ((==0) . (n`mod`)) [1..n])
        triangleNumbers = scanl1 (+) [1..]