module Draw where
  import Graphics.Gloss

  tile :: Float
  tile = 10

  getGrid :: Int -> Int -> [Point]
  getGrid n m = zip xs ys where
    xs = fromIntegral <$> (\x -> x `mod` n * 10) <$> fromIntegral <$> [0..m]
    ys = fromIntegral <$> (\y -> y `div` n * 10) <$> fromIntegral <$> [0..m-1]

  splitEvery :: Int -> [a] -> [[a]]
  splitEvery _ [] = []
  splitEvery n xs = let (a,b) = splitAt n xs in a : splitEvery n b

  applyHeight :: Float -> [Point] -> [Int] -> [Point]
  applyHeight n = zipWith (\(a,b) c -> (a, (b + fromIntegral c * n))) where

  applyIso :: [Point] -> [Point]
  applyIso xs = map (\(a,b) -> ((a-b) * tile/2, (a+b) * tile/2)) xs

  draw :: [Path] -> Picture
  draw path = pictures $ map ((color white) . line) path