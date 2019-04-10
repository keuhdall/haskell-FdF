module Checker (validateInput) where
	import Data.Char

	transformInput :: [[String]] -> Maybe [[Int]]
	transformInput xs
		| all isNumber (concat xs) == False = Nothing
		| otherwise = Just $ map (\x -> map read x) xs where
			isNumber str = case (reads str) :: [(Double, String)] of
				[(_, "")]	-> True
				_			-> False

	validateLineSize :: [[String]] -> Bool
	validateLineSize lines
		| length lines <= 0 = False
		| otherwise = let lineSize = length (lines !! 0) in (length lines) == length (filter (\x -> length x == lineSize) lines)

	validateInput :: [[String]] -> Maybe [[Int]]
	validateInput xs
		| validateLineSize xs == False = Nothing
		| otherwise = transformInput xs