alph = ['A'..'Z']
names = getNameList >>= return . zip [1..]

main :: IO ()
main = undefined

scoreName :: String -> Int
scoreName name = do
  names <- names
  let s = sum $ map (getSum 0) name
  let p = findPosition 0
  s * p
  where getSum n c 
          | alph !! n == c = n + 1
          | otherwise      = getSum (n+1) c
        findPosition n
          | snd (names !! n) == name = fst (names !! n)
          | otherwise                = findPosition (n+1)

getNameList :: IO [String]
getNameList = do
  let removeChar '"'  = False
      removeChar '\\' = False
      removeChar  _   = True
  let addSpace   ','  = ' '
      addSpace    c   = c
  readFile "names.txt" >>= return . qsort . words . map addSpace . filter removeChar

qsort :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (p:xs) = qsort[x | x<-xs, x <= p] ++ [p] ++ qsort[x | x<-xs, x > p] 