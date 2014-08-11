main :: IO ()
main = print . sum . listify $ factorial 100
   where factorial n
            | n == 0    = 1
            | otherwise = n * factorial(n-1) 

listify :: Integer -> [Integer]
listify n 
   | n == 0    = []
   | otherwise = listify (n `quot` 10) ++ [n `mod` 10]