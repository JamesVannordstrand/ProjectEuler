main :: IO()
main = print $ largestFactor 600851475143

largestFactor :: Integer -> Integer
largestFactor n = head $ filter (\x -> n `mod` x == 0) [x | x <- [n - 1, (n-2)..2], isPrime x]

isPrime :: Integer -> Bool
isPrime x = null [y | y<-[2..floor . sqrt $ fromIntegral x], x `mod` y == 0]