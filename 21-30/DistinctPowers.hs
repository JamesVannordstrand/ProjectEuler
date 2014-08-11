import Data.List

n1 = [2..100]
n2 = [2..100]

main :: IO ()
main = print $ length nums

nums :: [Double]
nums = nub $ concatMap inner n1
  where inner n = map (**n) n2  