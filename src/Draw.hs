module Draw where
	import Graphics.Gloss

	tile :: Float
	tile = 10

	getGrid :: Int -> Int -> [Point]
	getGrid n m = zip xs ys where
		xs = map (\x -> fromIntegral $ x `mod` n * 10) [0..m]
		ys = map fromIntegral $ map (\y -> (floor . fromIntegral) $ y `div` n * 10) [0..m-1]

	splitEvery :: Int -> [a] -> [[a]]
	splitEvery _ [] = []
	splitEvery n xs = let x = splitAt n xs in (fst x) : splitEvery n (snd x)

	hLines :: Int -> [Point] -> [[Point]]
	hLines n xs = splitEvery n xs

	applyHeight :: Int -> [Point] -> [Int] -> [Point]
	applyHeight n xs ys = zipWith (\x y -> (fst x, snd x + (fromIntegral y*5))) xs ys

	applyIso :: [Point] -> [Point]
	applyIso xs = map (\x -> ((fst x - snd x)*tile/2, (fst x + snd x)*tile/2)) xs

	draw :: [Path] -> Picture
	draw path = let pics = map ((color white) . line) path in pictures pics