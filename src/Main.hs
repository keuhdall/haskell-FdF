module Main where

import Graphics.Gloss
import System.IO
import System.Environment
import Draw
import Checker

window :: Display
window = InWindow "Fdf" (800, 600) (20, 20)

background :: Color
background = black

main :: IO ()
main = do
	args <- getArgs
	content <- readFile (args !! 0)
	let fileContent = lines content
	let workingContent = map words fileContent in
		case validateInput workingContent of
			Just validInput -> do
				let lineLength = length $ validInput !! 0
				let isoGrid = applyIso $ getGrid lineLength $ length $ concat validInput
				let points = applyHeight 5 isoGrid $ concat $ reverse validInput
				display window background $ draw $ (hLines lineLength points) ++ (vLines $ hLines lineLength points)
			Nothing -> putStrLn "KO"