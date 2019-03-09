module Checker where
	import Data.Char

	transformInput :: [[String]] -> Maybe [[Int]]
	transformInput xs
		| all isDigit (concat (concat xs)) == False = Nothing
		| otherwise = Just $ map (\x -> map (\y -> read y) x) xs

	validateLineSize :: [[String]] -> Bool
	validateLineSize lines
		| length lines <= 0 = False
		| otherwise = let lineSize = length (lines !! 0) in (length lines) == length (filter (\x -> length x == lineSize) lines)

	validateInput :: [[String]] -> Maybe [[Int]]
	validateInput xs
		| validateLineSize xs == False = Nothing
		| otherwise = transformInput xs