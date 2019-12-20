module Checker (validateInput) where
  import Data.Char (isNumber)

  transformInput :: [[String]] -> Maybe [[Int]]
  transformInput xs
    | all isNumber (concat . concat $ xs) == False = Nothing
    | otherwise = Just $ (map read) <$> xs

  validateLineSize :: [[String]] -> Bool
  validateLineSize lns = if null lns then False else length lns == (length $ filter (\x -> length x == lineSize) lns) where
    lineSize = length $ lns !! 0

  validateInput :: [[String]] -> Maybe [[Int]]
  validateInput xs = if validateLineSize xs == False then Nothing else transformInput xs