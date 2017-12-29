module Field
( field
, isField
, width
, height
, grad
, endIndex
) where

import Control.Exception
import Field.Internal
import Cell

 
field :: String -> [[Cell String]]
field xs =  
      let f = lines xs
      in  [createRow n | n <- f]

width :: [[Cell String]] -> Int
width [] = assert (False) 0
width (x:xs) = assert (isField (x:xs)) $ length x    

height :: [[Cell String]] -> Int
height x = width x

grad :: [[Cell String]] -> Int
grad xs = assert (isSquare xs) $
      let Just width' = (checkWidths xs)
          root = (sqrt (fromIntegral width'::Float))
      in (floor root)::Int
  
isField :: [[Cell String]] -> Bool 
isField [] = False
isField  xs = (isSquare xs) && (isLineSquare xs)

endIndex :: [[Cell String]] -> Int
endIndex xs = assert (isField xs) $
      (width xs)-1
