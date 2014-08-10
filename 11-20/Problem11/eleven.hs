main :: IO()
main = grid >>= groupV

--largestProduct :: IO Int
--largestProduct = do
--	let largestDiagnal = undefined
--	largestVertical <- grid >>= return . maximum . groupV
--	let largestHorizantal = undefined
--	return $ maximum [0, largestVertical, 0]

--groupV :: [[Int]] -> [Int]
groupV g = do
	let indexedGrid = zip [0..] $ map (zip [0..]) g 
	let rowGroup = [[snd w, snd x, snd y, snd z] | 
	               w <- indexedGrid, 
	               x <- indexedGrid, 
	               y <- indexedGrid, 
	               z <- indexedGrid, 
	               fst x <= fst w + 3 && fst x > fst w,
	               fst y <= fst w + 3 && fst y > fst w,
	               fst z <= fst w + 3 && fst z > fst w]
	print rowGroup
	--let colGroup = [snd w * snd x * snd y * snd z | 
	--							 w <- ]

grid :: IO[[Int]]
grid = do
	let repl ' ' = ','
	    repl  c  =  c
	let addBrackets line = '[' : line ++ "]"
	let convertToInt line = read line :: [Int]
	readFile "grid.txt" >>= return . map (convertToInt . addBrackets . map repl) . lines