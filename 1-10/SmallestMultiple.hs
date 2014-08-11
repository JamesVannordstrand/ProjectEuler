main :: IO()
main = print $ smallestDivided 20

smallestDivided :: Int -> Int
smallestDivided n = smallestDivided' n 1
  where smallestDivided' 0 b = b 
        smallestDivided' a b 
          | b `mod` a == 0 = smallestDivided' (a-1) b
          | otherwise      = smallestDivided' n (b+1)