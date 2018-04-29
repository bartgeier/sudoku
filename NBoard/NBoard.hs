module NBoard
( Square(..)
, canditates
, emptyBoard
,numbering
,puzzel
,create
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
      let sheet = numbering 0 . emptyBoard
          owners = own (puzzel str) . sheet
      in  mapBoard (owners str) (sheet str)

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

puzzel :: String -> [String] 
puzzel str =   
      lines [if x == ',' then '\n' else x  | x <- str, x /= '\n', x /= ' ' ]


own::[String]->Board->Board      
own _ [] = []  
own (p:ps) (b:board)
      | p == [] = own ps board
      | otherwise = [b {owner = p, candidates = []}] ++ own ps board
 
mapBoard :: [Square] -> Board -> Board
mapBoard s b = map (\m -> cell s m)b

cell :: [Square] -> Square -> Square
cell [] b = b
cell (x:xs) boardCell = 
      let newBoardCell = nakedSingle x boardCell
      in cell xs newBoardCell

nakedSingle:: Square -> Square -> Square
nakedSingle s b = 
      if (row s) == (row b) && (column s) == (column b) 
            then             
            b {candidates = (delete (owner s) (candidates b)), owner = (owner s) }
      else 
      if ((row s) == (row b) && (column s) /= (column b)
      ||  (row s) /= (row b) && (column s) == (column b)
      ||  (block s) == (block b)) 
            then 
            b {candidates = (delete (owner s) (candidates b))}
      else 
            b 

            
      
canditates :: String -> [String]
canditates string =
      let strings = nub (lines [if chr == ',' then '\n' else chr  | chr <- string, chr /= '\n', chr /= ' ' ])
      in [c | c <- strings, c /= ""]
      
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