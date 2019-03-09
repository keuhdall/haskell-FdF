module Main where

import Graphics.Gloss
import System.IO
import System.Environment
import Data.Char

offset :: Float
offset = 50

window :: Display
window = InWindow "Fdf" (800, 600) (20, 20)

background::Color
background = black

drawing :: [Path] -> Picture
drawing path = let pics = map (\x -> color white (line x)) path in pictures pics

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

hLines :: [[Int]] -> [[Point]]
hLines yss = zipWith valueToGrid xs $ reverse yss where
	valueToGrid x y = zipWith3 (\k l m -> (offset*k, l+m)) xs (map (\y' -> fromIntegral y') y) (cycle [offset*x])
	xs = map (\x -> fromIntegral x) [0..]

vLines :: [[Point]] -> [[Point]]
vLines xss = vLines' len xss [] where
	len = (length (xss !! 0)) - 1
	vLines' n xss acc
		| n >= 0 = vLines' (n-1) xss ((map (\xs -> (xs !! n)) xss) : acc)
		| otherwise = acc

main :: IO ()
main = do
	args <- getArgs
	content <- readFile (args !! 0)
	let fileContent = lines content
	let workingContent = map (\x -> words x) fileContent in
		case validateInput workingContent of
			Just validInput -> do
				let points = (hLines validInput) ++ (vLines $ hLines validInput)
				display window background $ drawing points
			Nothing -> putStrLn "KO"