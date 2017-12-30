module SortedBacktrack
( candidates

) where

import Board
import Data.List
import Rule

sortedTrack :: [[Cell String]]  -> (Maybe Int, Maybe Int) -> [Cell String] 
      -> ((Int,Int), [Cell String])
sortedTrack board (x,y) dictonary = 
      sortby ordNumCanditates (track board (x,y) dictonary)
      sortby (\(_,a) -> (_,b) -> 
            compare (length a) (length b)
      ) (track board (x,y) dictonary)
      
--ordNumCanditates :: ((Int,Int), [Cell String]) -> ((Int,Int), [Cell String]) -> Ordering            
--ordNumCanditates (_, b1) (_, b2)  = compare (length b1) (length b2)      

track :: [[Cell String]]  -> (Maybe Int, Maybe Int) -> [Cell String] 
      -> ((Int,Int), [Cell String])
track board (ix,iy) dictonary
      | (mX,mY) == (Nothing,Nothing) = candidates  board (ix,iy) dictonary
      otherwise = (candidates  board (ix,iy) dictonary) ++ (track board (mX,mY) dictonary)
      where 
            (mX,mY) = next board (ix,iy)


  
 

candidates :: [[Cell String]]  ->(Int,Int) -> [Cell String] 
     -> ((Int,Int), [Cell String])
candidates  board (x,y) dictonary =
      let candidates'  = [[candidate | candidate <- dictonary, (allowed board (x,y) candidate)]
      in  (x,y), candidates')
       
--length candidates     

{-
(mX,mY) = step board (ix,iy)

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
-}