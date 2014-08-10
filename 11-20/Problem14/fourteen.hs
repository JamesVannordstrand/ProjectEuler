import Data.List

main :: IO ()
main = print fstHalf 
  where maxn n lengths = head . sortBy (flip compare) $ zip lengths [n..]
        fstHalf = maxn 1 $ map (length . callatzSeq) [1..999999]

callatzSeq :: Int -> [Int]
callatzSeq n
  | n == 1 = [1]
  | even n = n : callatzSeq (n`quot`2)
  | otherwise = n : callatzSeq (3 * n + 1)