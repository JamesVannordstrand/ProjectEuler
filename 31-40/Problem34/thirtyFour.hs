module Main where

listify :: Int -> [Int]
listify n 
   | n == 0    = []
   | otherwise = listify (n `quot` 10) ++ [n `mod` 10]

factorial :: Int -> Int
factorial n = if n == 0 then 1 else n * factorial (n - 1)

main :: IO ()
main = print . sum $ filter (\element -> element == (sum . map factorial $ listify element)) [3..100000]
