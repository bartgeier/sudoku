module BruteforceBacktrack.Internal 
( nextCell
, setNextCanditate
) where

import Board
import Data.List
import Rule

setNextCanditate :: [[Cell String]] -> (Int, Int) -> [Cell String] 
        -> ([[Cell String]], [[Cell String]] -> (Maybe Int, Maybe Int) -> (Maybe Int, Maybe Int))       
setNextCanditate board (x,y) dictonary
      | candidates == [] = ((replaceXY board (x,y) Empty), back)
      | otherwise   = (newboard, next)
      where 
            (_,z2s) = nextCell dictonary board (x,y)
            candidates  = [cell' | cell' <- z2s, (allowed board (x,y) cell')]
            [candidate] = take 1 candidates
            newboard = replaceXY board (x,y) candidate 

nextCell :: [Cell String] -> [[Cell String]] -> (Int, Int)  
         -> ([Cell String],[Cell String])    
nextCell dictonary board (x,y) 
      | i ==  Nothing = splitAt 0 dictonary
      | otherwise     = splitAt (i'+1) dictonary
      where 
            cell = board `atXY` (x,y)
            i = findIndex (==cell) dictonary
            Just i' =  i 
            
{-

(newBoard, newStep) = setNextCanditate(board (x,y) dictonary)
      cell = nextCell(dictonary, board, (x,y));
      while (cell != Empty) {
            if (allowed(board, (x,y), cell)) {
                  board = replaceXY(board,(x,y), cell);
                  return (newBoard, next);
            }
      }    
      newBoard = (newBoard, back);
      return newBoard;
}
-}            