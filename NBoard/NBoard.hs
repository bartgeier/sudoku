module NBoard
( Square(..)
, canditates
, emptyBoard
) where

import Data.List

data Square = Square { row :: Int  
                     , column :: Int  
                     , block :: Int   
                     , owner :: String  
                     , candidates :: [String]  
                     } deriving (Show)   

type Board = [Square]
type Puzzel = [Square]

create :: String -> Board
create str = 
      let f = map (getPuzzelTable str) . numbering 0 . emptyBoard
      in f str

emptyBoard :: String -> Board
emptyBoard str =
      let numOfRow = length [lineFeed | lineFeed <- str, lineFeed == '\n']
          numOfSquar = length [squa | squa <- str, squa == ',']
          numOfColumn =  quot numOfSquar numOfRow -- numOfSquar / numOfRow         
          s = Square {row = numOfRow
                     ,column = numOfColumn
                     ,block = 0
                     ,owner = ""
                     ,candidates = canditates str}
      in take numOfSquar (repeat s)

getPuzzelTable :: String -> Puzzel 
getPuzzelTable str =     
     
canditates :: String -> [String]
canditates str =
      let xs = nub (lines [if x == ',' then '\n' else x  | x <- str, x /= '\n', x /= ' ' ])
      in [c | c <- xs, c /= ""]
      
numbering :: Int -> Board -> Board  
numbering _ [] = []  
numbering idx (Square{row = r, column = c,owner = o, candidates = cs }:board) = 
      let r' = idx `quot` c
          c' = idx `mod` c
          b' = (r' `quot` (intSqrt r)) * (intSqrt c) + (c' `quot` (intSqrt c))      
          s = Square {row = r', column = c', block = b', owner = o, candidates = cs}
      in [s] ++ (numbering (idx+1) board )

intSqrt :: Int -> Int
intSqrt x = floor . sqrt $ (fromIntegral x :: Float)            