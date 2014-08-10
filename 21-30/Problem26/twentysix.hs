import Data.List

main :: IO ()
main = print $ map pattern fractions  

pattern :: String -> String
pattern word
  | length (nub word) == length word = word
  | comparing (nub word) word = nub word
  | comparing (tail $ nub word) (tail word) = tail $ nub word
  | otherwise = error "poop"   

comparing :: String -> String -> Bool
comparing _ [] = True
comparing word everything
  | word == take (length word) everything = comparing word (drop (length word) everything) 
  | otherwise = False


fractions :: [String]
fractions = map (tail . dropWhile (\x-> x /= '.') . show . (1/)) [2..10] 