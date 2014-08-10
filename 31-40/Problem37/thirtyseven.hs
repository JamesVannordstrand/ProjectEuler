module Main where

listify :: Int -> [Int]
listify n 
   | n == 0    = []
   | otherwise = listify (n `quot` 10) ++ [n `mod` 10]

listToNum :: [Int] -> Int
listToNum = foldl1 ((+) . (*10))

isPrime :: Int -> Bool
isPrime x = null [y | y <- [2..floor . sqrt $ fromIntegral x], x `mod` y == 0]

checkTruncationLeft :: [Int] -> Bool
checkTruncationLeft [] = True
checkTruncationLeft (x:xs)
  | isPrime (listToNum (x:xs)) = checkTruncationLeft xs
  | otherwise = False

checkTruncationRight :: [Int] -> Bool
checkTruncationRight [] = True
checkTruncationRight ls
  | isPrime (listToNum ls) = checkTruncationRight $ init ls
  | otherwise = False

main :: IO ()
main = print $ filter (\e -> (checkTruncationRight $ listify e) && (checkTruncationLeft $ listify e)) [8..10000]