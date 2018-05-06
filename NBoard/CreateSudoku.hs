module CreateSudoku
(module SudokuTypes
,create
,canditates
) where

import Data.List
import SudokuTypes

create :: String -> Board
create str = 
      let grid = (numbering 0 . emptyBoard) str
          givens = setOfKnowns (puzzel str) grid
      in  mapBoard nakedSingle givens grid
      
numbering :: Int -> Board -> Board  
numbering _ [] = []  
numbering counter (Square{row = r, column = c,owner = o, candidates = cs }:board) = 
      let i' = counter
          r' = counter `quot` c
          c' = counter `mod` c
          b' = (r' `quot` (intSqrt r)) * (intSqrt c) + (c' `quot` (intSqrt c))      
          s = Square {idx = i', row = r', column = c', block = b', owner = o, candidates = cs}
      in [s] ++ (numbering (counter+1) board )
      
intSqrt :: Int -> Int
intSqrt x = floor . sqrt $ (fromIntegral x :: Float)          

puzzel :: String -> [String] 
puzzel str =   
      lines [if x == ',' then '\n' else x  | x <- str, x /= '\n', x /= ' ' ]

setOfKnowns::[String]->Board->PatternList      
setOfKnowns _ [] = [] 
setOfKnowns [] _ = [] 
setOfKnowns (p:ps) (b:board)
      | p == [] = setOfKnowns ps board
      | otherwise = [[b {owner = p, candidates = []}]] ++ setOfKnowns ps board    

emptyBoard :: String -> Board
emptyBoard str =
      let numOfRow = length [lineFeed | lineFeed <- str, lineFeed == '\n']
          numOfSquar = length [squa | squa <- str, squa == ',']
          numOfColumn =  quot numOfSquar numOfRow          
          s = Square {idx = 0
                     ,row = numOfRow
                     ,column = numOfColumn
                     ,block = 0
                     ,owner = ""
                     ,candidates = canditates str}
      in take numOfSquar (repeat s)    

canditates :: String -> [String]
canditates string =
      let strings = nub (lines [if chr == ',' then '\n' else chr  | chr <- string, chr /= '\n', chr /= ' ' ])
      in [c | c <- strings, c /= ""]
       