main :: IO()
main = largeSum >>= print . take 10 . show 

largeSum :: IO Integer
largeSum = bigNumbers >>= return . sum

bigNumbers :: IO [Integer]
bigNumbers = do
   let convertToInt line = read line :: Integer
   readFile "number.txt" >>= return . map (convertToInt . init) . lines