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

drawing :: [Path] -> [Path] -> Picture
drawing hPath vPath = let pics = map (\x -> color white (line x)) (vPath ++ hPath) in pictures pics

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

hLines :: [[Int]] -> [[Point]]
hLines yss = zipWith valueToGrid xs yss where
	valueToGrid x y = zipWith3 (\k l m -> (offset*k, l+m)) xs (map (\y' -> fromIntegral y') y) (cycle [offset*x])
	xs = map (\x -> fromIntegral x) [0..]

vLines :: [[Point]] -> [[Point]]
vLines xss = vLines' (length xss) xss [] where
	vLines' n xss acc
		| n >= 0 = vLines' (n-1) xss ((map (\xs -> (xs !! n)) xss) : acc)
		| otherwise = acc

printPoints :: [[Point]] -> IO ()
printPoints xs = let values = concat xs in mapM_ (\x -> putStr ("(" ++ (show (fst x)) ++ ", " ++ (show (snd x)) ++ ")" ++ " ")) values

main :: IO ()
main = do
	args <- getArgs
	content <- readFile (args !! 0)
	let fileContent = lines content
	let workingContent = map (\x -> words x) fileContent in
		case validateInput workingContent of
			Just validInput -> do
				printPoints $ vLines (hLines validInput)
				mapM_ (\x -> putStrLn (show (length x))) (vLines (hLines validInput))
				putStr "\n"
				display window background (drawing (hLines validInput) (vLines (hLines validInput)))
			Nothing -> putStrLn "KO"