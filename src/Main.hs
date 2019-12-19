module Main where

import Graphics.Gloss
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.List
import Draw
import Checker

window :: Display
window = InWindow "Fdf" (800, 600) (20, 20)

background :: Color
background = black

tileHeight :: Float
tileHeight  = 5

printHelp :: IO ()
printHelp = putStrLn $ "Please provide at least one argument.\n \
\ Usage : hs-FdF [file]"

checkArgs :: [String] -> IO [String]
checkArgs xs = if null xs then printHelp >> exitSuccess else pure xs

getComputedGrid :: [[Int]] -> Maybe [[Point]]
getComputedGrid xss = Just $ pointLines ++ transpose pointLines where
  size = length $ xss !! 0
  points = applyHeight tileHeight (applyIso . getGrid size . length $ concat xss) . concat $ reverse xss
  pointLines = splitEvery size points

main :: IO ()
main = do
  args <- checkArgs =<< getArgs
  content <- readFile (args !! 0)
  let fileContent = lines content
  let workingContent = map words fileContent
  case validateInput workingContent >>= getComputedGrid of
    Just (points) -> display window background $ draw points
    Nothing -> putStrLn "Invalid input, please check your file!"