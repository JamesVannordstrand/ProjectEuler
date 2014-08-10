main :: IO()
main = print $ smallestDivided 20

smallestDivided :: Int -> Int
smallestDivided n = smallestDivided' n 1
  where smallestDivided' 0 b = b 
        smallestDivided' a b 
          | b `mod` a == 0 = smallestDivided' (a-1) b
          | otherwise      = smallestDivided' n (b+1)


--much faster solution/simpler
--print $ foldr1 lcm [1..20]
--folds least common multiplier over 1..20