module Main where

listify :: Int -> [Int]
listify n 
   | n == 0    = []
   | otherwise = listify (n `quot` 10) ++ [n `mod` 10]

palindrome :: [Int] -> Bool
palindrome ls = reverse ls == ls

toBin :: Int -> [Int]
toBin 0 = []
toBin n 
  | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
  | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

main :: IO ()
main = print $ foldl checkAdd 0 $ filter (palindrome . listify) [1..999999]
  where checkAdd acc element 
          | palindrome (toBin element) = acc + element  
          | otherwise = acc + 0