module Checker (validateInput) where
  import Data.Char (isNumber)

  transformInput :: [[String]] -> Maybe [[Int]]
  transformInput xs
    | all isNumber (concat . concat $ xs) == False = Nothing
    | otherwise = Just $ (map read) <$> xs where

  validateLineSize :: [[String]] -> Bool
  validateLineSize lns = let lineSize = length (lns !! 0) in
    if null lns then False else length lns == (length $ filter ((\x -> length x == lineSize)) lns)

  validateInput :: [[String]] -> Maybe [[Int]]
  validateInput xs = if validateLineSize xs == False then Nothing else transformInput xs