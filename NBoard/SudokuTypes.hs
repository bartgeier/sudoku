module SudokuTypes
(module SudokuTypes
) where

import Data.List

data Square = Square { idx :: Int
                     , row :: Int  
                     , column :: Int  
                     , block :: Int   
                     , owner :: String  
                     , candidates :: [String]  
                     } deriving (Show)
                 
type Board = [Square]
type Pattern = [Square]
type PatternList = [Pattern] 
type Strategy = (Pattern -> Square -> Square)

mapBoard :: Strategy -> PatternList -> Board -> Board
mapBoard strategy set board =  
      let function = action strategy set
      in map (\m -> function m)board
      
action :: Strategy -> PatternList -> Square -> Square
action _ [] b = b
action strategy (x:xs) boardCell = 
      let cell = strategy x boardCell
      in action strategy xs cell

--strategy
nakedSingle :: Strategy
nakedSingle [] _ = error "(nakedSingle [] _)"
nakedSingle (_:_:_) _ = error "(nakedSingle (_:_:_) _ )"
nakedSingle [setCell] boardCell = 
      if (row setCell) == (row boardCell) && (column setCell) == (column boardCell) then                          
            boardCell {owner = (owner setCell)
                      ,candidates = (delete (owner setCell) (candidates boardCell))}
      else 
      if ((row setCell) == (row boardCell) && (column setCell) /= (column boardCell)
      ||  (row setCell) /= (row boardCell) && (column setCell) == (column boardCell)
      ||  (block setCell) == (block boardCell)) then             
            boardCell {candidates = (delete (owner setCell) (candidates boardCell))}
      else 
            boardCell 