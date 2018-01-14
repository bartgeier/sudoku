module Rule.Internal
( column
, row 
, blockRule
) where

import Data.List
import Board
import Tool_List

column  :: Field -> Int -> Cell String -> Bool  
column xs idx cell = row (transpose xs) idx cell
      
      
row :: Field -> Int -> Cell String -> Bool  
row board idx  cell  
      | findIndex (==cell) row' == Nothing = False
      | findIndex (==cell) row' /= Nothing = True 
      | otherwise = False
      where row' = board `at` idx   
     
blockRule :: Field -> Int -> Int -> Cell String -> Bool
blockRule board idxWith idxRow cell =
      let blocks' = blocks board
          idx = blockIdx (grad board) idxWith
          idy = blockIdx (grad board) idxRow
          br = blocks' `at` idy 
      in row br idx cell

