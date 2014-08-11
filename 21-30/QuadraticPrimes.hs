module Main where

main :: IO ()
main = print $ maximumProduct [(consecutivePrimes [0..] 0 a b, (a, b)) | a <- [0..1000], b <- [0..1000]]

maximumProduct :: [(Int, (Int, Int))] -> Int
maximumProduct ls = do
  let m = maxTail ls
  (fst $ snd m) * (snd $ snd m)
  where maxTail [x] = x
        maxTail (x:xs)
          | fst x > fst (maxTail xs) = x
          | otherwise = maxTail xs

consecutivePrimes :: [Int] -> Int -> Int -> Int -> Int
consecutivePrimes [] size a b = size
consecutivePrimes (x:xs) size a b
  | isPrime (formula a b x) = consecutivePrimes xs (size+1) a b
  | otherwise = size 
  where isPrime x = null [y | y <- [2..floor . sqrt $ fromIntegral x], x `mod` y == 0]


formula :: Int -> Int -> Int -> Int
formula a b n = n ^ 2 + (-a) * n + b 

