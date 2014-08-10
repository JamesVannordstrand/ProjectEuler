main :: IO ()
main = print . fst . head . filter (\x -> count 0 (snd x) == 100) $ zip [1..] fibs
  where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
        count c num 
          | num < 1 = c 
          | otherwise = count (c + 1) (num `quot` 10) 