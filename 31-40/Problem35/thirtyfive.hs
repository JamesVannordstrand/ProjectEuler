module Main where

listToNum :: [Int] -> Int
listToNum = foldl1 ((+) . (*10))

listify :: Int -> [Int]
listify n 
   | n == 0    = []
   | otherwise = listify (n `quot` 10) ++ [n `mod` 10]

rotatedNums :: [Int] -> [[Int]]
rotatedNums nums = rotatedNums' (last nums : init nums) nums
  where rotatedNums' rotation original
          | rotation == original = []
          | otherwise = rotation : rotatedNums' (last rotation : init rotation) original 

isPrime :: Int -> Bool
isPrime x = null [y | y<-[2..floor . sqrt $ fromIntegral x], x `mod` y == 0]

listOfPrimes :: [Int]
listOfPrimes = filter isPrime [2..999999]

checkRotations :: [[Int]] -> Bool
checkRotations [] = True
checkRotations (x:xs) 
  | isPrime (listToNum x) = checkRotations xs
  | otherwise = False 

main :: IO ()
main = print . length $ filter (checkRotations . rotatedNums . listify) listOfPrimes