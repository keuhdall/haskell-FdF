module Main where

import Graphics.Gloss
import System.IO
import System.Environment

window :: Display
window = InWindow "Fdf" (800, 600) (20, 20)

background::Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = do
	args <- getArgs
	content <- readFile (args !! 0)
	let fileContent = lines content in
		mapM_ (\x -> putStrLn x) fileContent
		--display window background drawing
