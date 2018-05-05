module SudokuTypes
(Square(..)
,Board
,Pattern
,PatternList
,Strategy
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
