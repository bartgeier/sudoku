module Strategys
(searchNakedSingles
,searchHiddenSingles
) where

import Data.List
import SudokuTypes


searchNakedSingles :: Board -> PatternList
searchNakedSingles board =
      [[x] | x <- board, nakedSingl x]

nakedSingl :: Square ->  Bool
nakedSingl Square{candidates = c} = 
      if length c == 1 then            
            True
      else
            False
 
searchHiddenSingles :: Board -> PatternList   
searchHiddenSingles board =
      [[Square{idx = 0 
      ,row = 1 
      ,column = 2
      ,block = 3
      ,owner = "2"
      ,candidates = ["2"] }]]
      
rowStatistic :: Int -> Board -> [(String,Int)] 
rowStatistic row board =
      [x | x <- board, countR x ]

countR :: Square
countR Square{candidates = c} =      