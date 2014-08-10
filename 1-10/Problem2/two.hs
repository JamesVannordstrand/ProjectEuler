main :: IO()
main = print $ sum [x | x <- takeWhile (<= 4000000) fibs, even x]
  where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

--main :: IO()
--main = print . sum . filter even $ fib

--fib :: [Int]
--fib = fib' [1,1]
--  where fib' (x:y:xs)  
--          | x+y > 4000000  = (x:y:xs)
--          | x+y <= 4000000 = fib'(x+y : (x:y:xs))