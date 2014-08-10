main :: IO()
main = print . sum . filter isPrime $ [2..1999999]

isPrime :: Int -> Bool
isPrime x = null [y | y <- [2..floor . sqrt $ fromIntegral x], x `mod` y == 0]