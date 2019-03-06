module Main where

import Graphics.Gloss
import System.IO
import System.Environment
import Data.Char

offset :: Float
offset = 20

window :: Display
window = InWindow "Fdf" (800, 600) (20, 20)

background::Color
background = black

drawing :: Picture
drawing = color white (line [(0, 0), (0, 100)])

collectStrings :: [String] -> [[String]]
collectStrings xs = map (\x -> words x) xs

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

printValues :: [[Int]] -> IO ()
printValues xs = let values = concat xs in mapM_ (\x -> putStr ((show x) ++ " ")) values

-- you can use zipWith to "map" over two lists in parallel
-- and use [0..] to generate the sequence [0,1,2,3,.....] to use as a counter
drawGrid :: [[Int]] -> [[Point]]
drawGrid yss = map (\ys -> zipWith valueToGrid xs ys) yss where
	valueToGrid x y = (offset + 20*x, offset + (fromIntegral y))
	xs = map (\x -> fromIntegral x) [0..]

printPoints :: [[Point]] -> IO ()
printPoints xs = let values = concat xs in mapM_ (\x -> putStr ("(" ++ (show (fst x)) ++ ", " ++ (show (snd x)) ++ ")" ++ " ")) values

main :: IO ()
main = do
	args <- getArgs
	content <- readFile (args !! 0)
	let fileContent = lines content
	let workingContent = collectStrings fileContent in
		case validateInput workingContent of
			Just validInput -> do
				printValues validInput
				putStr "\n"
				printPoints $ drawGrid validInput
				putStr "\n"
				display window background drawing
			Nothing -> putStrLn "KO"