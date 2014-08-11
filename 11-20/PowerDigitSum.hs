main :: IO()
main = print . sum $ listify (2^1000)

listify :: Integer -> [Integer]
listify n 
   | n == 0    = []
   | otherwise = listify (n `quot` 10) ++ [n `mod` 10]