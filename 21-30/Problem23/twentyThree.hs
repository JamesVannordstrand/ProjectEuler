import Data.List 

ls = [1..28123]

main :: IO ()
main = print $ sum ls - addUp listOfAbundants

addUp :: [Int] -> Int
addUp (x:y:xs)  =  sum (nub $ map (+x) (y:xs)) + addUp (y:xs)
addUp  _        =  0
 
listOfAbundants :: [Int]
listOfAbundants = filter isAbundant ls
  where isAbundant x
           | x < sum [y | y <- [1.. x `quot` 2], x `mod` y == 0] = True
           | otherwise = False