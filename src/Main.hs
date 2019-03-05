module Main where

import Graphics.Gloss
import System.IO
import System.Environment
import Data.Char

window :: Display
window = InWindow "Fdf" (800, 600) (20, 20)

background::Color
background = white

drawing :: Picture
drawing = circle 80

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

main :: IO ()
main = do
	args <- getArgs
	content <- readFile (args !! 0)
	let fileContent = lines content
	let workingContent = collectStrings fileContent in
		case validateInput workingContent of
			Just validInput	-> do
				printValues validInput
				putStr "\n"
			Nothing			-> putStrLn "KO"