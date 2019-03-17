module Draw where
	import Graphics.Gloss

	tile :: Float
	tile = 10

	getGrid :: Int -> Int -> [Point]
	getGrid n m = zipWith (\a b -> (a, b)) xs ys where
		xs = map (\x -> fromIntegral (x `mod` n * 10)) [0..m]
		ys = map (\y' -> fromIntegral y') $ map (\y -> (floor . fromIntegral) (y `div` n * 10)) [0..m-1]

	splitEvery :: Int -> [a] -> [[a]]
	splitEvery _ [] = []
	splitEvery n xs = let x = splitAt n xs in (fst x) : splitEvery n (snd x)

	hLines :: Int -> [Point] -> [[Point]]
	hLines n xs = splitEvery n xs

	vLines :: [[Point]] -> [[Point]]
	vLines xss = vLines' len xss [] where
		len = (length (xss !! 0)) - 1
		vLines' n xss acc
			| n >= 0 = vLines' (n-1) xss ((map (\xs -> (xs !! n)) xss) : acc)
			| otherwise = acc

	applyHeight :: Int -> [Point] -> [Int] -> [Point]
	applyHeight n xs ys = zipWith (\x y -> (fst x, snd x + (fromIntegral y*5))) xs ys

	applyIso :: [Point] -> [Point]
	applyIso xs = map (\x -> ((fst x - snd x)*tile/2, (fst x + snd x)*tile/2)) xs

	draw :: [Path] -> Picture
	draw path = let pics = map (\x -> color white (line x)) path in pictures pics