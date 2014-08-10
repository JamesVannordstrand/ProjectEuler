import Data.List

main :: IO()
main = print $ largestPallyNDigits 100 999

largestPallyNDigits :: Int -> Int -> Int
largestPallyNDigits lower upper = maximum [x*y | x <- [lower..upper], y <- [lower..upper], show (x*y) == reverse (show (x*y))]