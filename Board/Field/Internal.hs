module Field.Internal
( createRow
, isSquare
, checkWidths
, isLineSquare
) where

import Data.List
import Data.Fixed
import Cell
       
createRow :: String -> [Cell String]
createRow xs = 
      let ls = lines [if x == ',' then '\n' else x | x <- xs, x /= ' ']
      in  [if l == "" then Empty else Fix l | l <- ls]
      
isSquare :: [[Cell String]] -> Bool
isSquare [] = False
isSquare xs
      | width' == height = True
      | otherwise = False
      where width' = checkWidths xs
            height = checkWidths (transpose xs)
            
checkWidths :: [[Cell String]] -> Maybe Int
checkWidths [] = Nothing
checkWidths (x:xs)
      | nextWidth == Nothing = width'
      | width' == nextWidth = width'
      | otherwise = notSameLength
      where width' = Just $ length x
            nextWidth = checkWidths xs
            notSameLength = Just 0

isLineSquare :: [[Cell String]] -> Bool
isLineSquare [] = False
isLineSquare (x:_)
      | rest == (0::Double) = True
      | otherwise = False
      where rest = (sqrt $ fromIntegral $ length x) `mod'` 1      





