module Draw where
	import Graphics.Gloss

	tile :: Float
	tile = 10

	hLines :: [[Int]] -> [[Point]]
	hLines yss = zipWith valueToGrid xs $ reverse yss where
		valueToGrid x y = zipWith3 (\k l m -> (tile*k, l+m)) xs (map (\y' -> fromIntegral y') y) (cycle [tile*x])
		xs = map (\x -> fromIntegral x) [0..]

	vLines :: [[Point]] -> [[Point]]
	vLines xss = vLines' len xss [] where
		len = (length (xss !! 0)) - 1
		vLines' n xss acc
			| n >= 0 = vLines' (n-1) xss ((map (\xs -> (xs !! n)) xss) : acc)
			| otherwise = acc

	applyIso :: [[Point]] -> [[Point]]
	applyIso xss = map (\xs -> map (\x -> ((fst x - snd x)*2, (fst x + snd x))) xs) xss

	draw :: [Path] -> Picture
	draw path = let pics = map (\x -> color white (line x)) path in pictures pics