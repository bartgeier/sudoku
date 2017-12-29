module BruteforceBacktrack 
( bruteforce
) where

import BruteforceBacktrack.Internal
import Board

bruteforce :: [[Cell String]] -> (Maybe Int, Maybe Int) 
      -> ([[Cell String]] -> (Maybe Int, Maybe Int) -> (Maybe Int, Maybe Int))
      -> [Cell String] 
      -> [[Cell String]]
bruteforce board (ix,iy) step dictonary 
      | mX  == Nothing   = board
      | fix cell == True = bruteforce board (mX,mY) step dictonary
      | otherwise        = bruteforce newBoard (mX,mY) newStep dictonary
      where
            (mX,mY) = step board (ix,iy)
            (Just x, Just y) = (mX,mY)
            cell = atXY board (x,y)
            (newBoard, newStep) = setNextCanditate board (x,y) dictonary

      
{-

newBoard = bruteforce(board, (x_,y_), step, dictonary) {
      (x,y) = step(board (x,y));
      if((x,y) == (Nothing,Nothing)) {
            return board;
      }
      if (notFix(atXY(board,(x,y)) {
            newBoard,step = setNextCanditate(board, (x,y), dictonary);
            return bruteforce(newBoard, (x,y), step, dictonary);
      } 
      return bruteforce(board, (x,y), step, dictonary);

}

-}
