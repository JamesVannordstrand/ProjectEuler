main :: IO()
main = print $ findPrime 10000

findPrime :: Int -> Int
findPrime num = last . take num $ filter isPrime [1..]

isPrime :: Int -> Bool
isPrime x = null [y | y<-[2..floor . sqrt $ fromIntegral x], x `mod` y == 0]