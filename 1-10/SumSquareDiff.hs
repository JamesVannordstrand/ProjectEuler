main :: IO()
main = print $ sumSquares 100

sumSquares :: Int -> Int
sumSquares n = (sum [1..n])^2 - sum [x^2 | x <- [1..n]]