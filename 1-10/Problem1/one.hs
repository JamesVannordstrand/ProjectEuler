import Data.List (union)

main :: IO()
main = print $ sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

-- sum . union [3, 1..1000] [5, 1..999]