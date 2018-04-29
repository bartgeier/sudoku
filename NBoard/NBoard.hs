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
type Pattern = [Square]
type Set = [Pattern] 
type Strategy = (Pattern -> Square -> Square)
--grid



create :: String -> Board
create str = 
      let grid = (numbering 0 . emptyBoard) str
          givens = setOfKnowns (puzzel str) grid
      in  mapBoard nakedSingle givens grid
      
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

puzzel :: String -> [String] 
puzzel str =   
      lines [if x == ',' then '\n' else x  | x <- str, x /= '\n', x /= ' ' ]

setOfKnowns::[String]->Board->Set      
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
          s = Square {row = numOfRow
                     ,column = numOfColumn
                     ,block = 0
                     ,owner = ""
                     ,candidates = canditates str}
      in take numOfSquar (repeat s)    

canditates :: String -> [String]
canditates string =
      let strings = nub (lines [if chr == ',' then '\n' else chr  | chr <- string, chr /= '\n', chr /= ' ' ])
      in [c | c <- strings, c /= ""]
      
------------------

      
mapBoard :: Strategy -> Set -> Board -> Board
mapBoard strategy set board =  
      let function = action strategy set
      in map (\m -> function m)board
      
action :: Strategy -> Set -> Square -> Square
action _ [] b = b
action strategy (x:xs) boardCell = 
      let cell = strategy x boardCell
      in action strategy xs cell

--strategy
nakedSingle :: Strategy
nakedSingle [] _ = error "(nakedSingle [] _)"
nakedSingle (_:_:_) _ = error "(nakedSingle (_:_:_) _ )"
nakedSingle [setCell] boardCell = 
      if (row setCell) == (row boardCell) && (column setCell) == (column boardCell) 
            then             
            boardCell {owner = (owner setCell)
                      ,candidates = (delete (owner setCell) (candidates boardCell))}
      else 
      if ((row setCell) == (row boardCell) && (column setCell) /= (column boardCell)
      ||  (row setCell) /= (row boardCell) && (column setCell) == (column boardCell)
      ||  (block setCell) == (block boardCell)) 
            then 
            boardCell {candidates = (delete (owner setCell) (candidates boardCell))}
      else 
            boardCell 
            



        