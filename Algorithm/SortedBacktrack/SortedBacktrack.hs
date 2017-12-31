module SortedBacktrack
( track
, candidates

) where

import Board
import Data.List
import Rule

createTrack :: [[Cell String]] -> [Cell String] 
      -> ([((Int,Int), [Cell String])],[((Int,Int), [Cell String])])
createTrack board dictonary = ([],(track board (Nothing,Nothing) dictonary))   

track :: [[Cell String]]  -> (Maybe Int, Maybe Int) -> [Cell String] 
      -> [((Int,Int), [Cell String])]
track _ (Nothing, Just _) _  = error "(Nothing, Just _)"    
track _ (Just _, Nothing) _  = error "(Just _, Nothing)"  
track board (ix,iy) dictonary
      | (mX,mY) == (Nothing,Nothing) = []
      | fix (atXY board (x,y)) == True = (track board (mX,mY) dictonary)
      | otherwise = 
            sortBy (\(_,a) (_,b) -> compare (length a) (length b)) 
                   ([(candidates  board (x,y) dictonary)] ++ (track board (mX,mY) dictonary))
      where 
            (mX,mY) = next board (ix,iy)
            (Just x, Just y) = (mX,mY)

candidates :: [[Cell String]] -> (Int, Int) -> [Cell String] 
     -> ((Int,Int), [Cell String])
candidates  board (x,y) dictonary
      | fix (atXY board (x,y)) == True = ((x,y), [])
      | otherwise = ((x,y), candidates')
      where
            candidates' = [candidate | candidate <- dictonary, (allowed board (x,y) candidate)]



          
    
       
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