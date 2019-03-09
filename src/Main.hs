module Main where

import Graphics.Gloss
import System.IO
import System.Environment
import Draw
import Checker

window :: Display
window = InWindow "Fdf" (800, 600) (20, 20)

background::Color
background = black

main :: IO ()
main = do
	args <- getArgs
	content <- readFile (args !! 0)
	let fileContent = lines content
	let workingContent = map (\x -> words x) fileContent in
		case validateInput workingContent of
			Just validInput -> do
				let points = applyIso $ (hLines validInput) ++ (vLines $ hLines validInput)
				display window background $ draw points
			Nothing -> putStrLn "KO"