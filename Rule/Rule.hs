module Rule
( notAllowed 
, allowed
) where

import Control.Exception
import Board
import Rule.Internal

allowed :: [[Cell String]] -> (Int, Int) -> Cell String -> Bool
allowed board xy cell = not (notAllowed board xy cell)

notAllowed :: [[Cell String]] -> (Int, Int) -> Cell String -> Bool
notAllowed board (x, y) cell = assert (isField board) $
      (column board x cell) || (row board y cell) || (blockRule board x y cell)
