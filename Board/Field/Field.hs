module Field
( Field
, field
, isField
, width
, height
, grad
, endIndex
) where

import Control.Exception
import Field.Internal
import Cell

type Field = [[Cell String]]
 
field :: String -> Field
field xs =  
      let f = lines xs
      in  [createRow n | n <- f]

width :: Field -> Int
width [] = assert (False) 0
width (x:xs) = assert (isField (x:xs)) $ length x    

height :: Field -> Int
height x = width x

grad :: Field -> Int
grad xs = assert (isSquare xs) $
      let Just width' = (checkWidths xs)
          root = (sqrt (fromIntegral width'::Float))
      in (floor root)::Int
  
isField :: Field -> Bool 
isField [] = False
isField  xs = (isSquare xs) && (isLineSquare xs)

endIndex :: Field -> Int
endIndex xs = assert (isField xs) $
      (width xs)-1
