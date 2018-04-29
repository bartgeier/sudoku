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
--type Pattern = [Square]
type Set = [Square]
type Strategie = (Set -> Square -> Square)

create :: String -> Board
create str = 
      let sheet = numbering 0 . emptyBoard
          setOfKnown = own (puzzel str) . sheet
      in  mapBoard nakedSingle (setOfKnown str) (sheet str)
      
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

own::[String]->Board->Board      
own _ [] = [] 
own [] _ = [] 
own (p:ps) (b:board)
      | p == [] = own ps board
      | otherwise = [b {owner = p, candidates = []}] ++ own ps board      

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

      
mapBoard :: Strategie -> Set -> Board -> Board
mapBoard strategie set board =  
      let function = action strategie set
      in map (\m -> function m)board
      
action :: Strategie -> Set -> Square -> Square
action _ [] b = b
action strategie (x:xs) boardCell = 
      let cell = strategie [x] boardCell
      in action strategie xs cell

--strategie      
nakedSingle :: Strategie
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

            



        