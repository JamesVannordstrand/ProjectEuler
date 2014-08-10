main :: IO ()
main = print . sum $ map count [1..1000]

count :: Int -> Int
count n
   | n < 14    = count1 n
   | n < 100   = count2 n
   | n <= 1000 = count3 n

count1 :: Int -> Int
count1 n
   | n == 1 || n == 2 || n == 6 || n == 10 = 3
   | n == 4 || n == 5 || n == 9 || n == 0  = 4
   | n == 3 || n == 7 || n == 8            = 5
   | n == 11 || n == 12                    = 6
   | n == 13                               = 8

count2 :: Int -> Int
count2 n
   | q == 1  = 4 + count1 r
   | q == 2 || q == 3 || q == 4 || q == 8 || q == 9 = 6 + count1 r
   | q == 5 || q == 6 = 5 + count1 r
   | q == 7 = 7 + count1 r
   | otherwise = 0 + count1 r 
   where q = n `quot` 10
         r = n `mod` 10

count3 :: Int -> Int
count3 n
   | q == 1 || q == 2 || q == 6 = 13 + count2 r
   | q == 4 || q == 5 || q == 9 = 14 + count2 r
   | q == 3 || q == 7 || q == 8 = 15 + count2 r
   | n == 1000 = 11
   where q = n `quot` 100
         r = n `mod` 100
