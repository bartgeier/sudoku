module SortedBacktrack
( candidates

) where

import Board
import Data.List
import Rule

track :: [[Cell String]]  ->(Int,Int) -> [Cell String] 
      -> (Int,Int)

candidates :: [[Cell String]]  ->(Int,Int) -> [Cell String] 
                -> ((Int,Int), [Cell String])
candidates  board (x,y) dictonary =
       let candidates'  = [[candidate | candidate <- dictonary, (allowed board (x,y) candidate)]
       in  ((x,y), candidates')
       
--length candidates     