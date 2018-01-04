module SortedBacktrack
( createSortedTrack
, sortedBacktrack
, candidates
, setNextCanditate
, nextCell


) where

import Board
import Data.List
import Rule
import Tool_List

 
type Track  = [ ((Int,Int), [Cell String]) ] 
type ZippedTrack  = (Track ,Track)

{-
sortedBacktrack :: [[Cell String]] 
      -> (ZippedTrack -> ZippedTrack)
      -> ZippedTrack
      -> [Cell String] 
      -> [[Cell String]]
sortedBacktrack _ _ (_,[]) = board      
sortedBacktrack board step track dictonary
      -- | end == [] = board
      -- | otherwise = sortedBacktrack newBoard newStep newTrack dictonary
      sortedBacktrack newBoard newStep newTrack dictonary
      where
            --(_, end) = track            
            (_,b) = track
            ((x,y), _) = head b
            --(_, ((x,y), _):pos) = newTrack            
            (newBoard, newStep) = setNextCanditate board (x,y) dictonary
            newTrack = newStep track
-}

sortedBacktrack :: [[Cell String]] 
      -> ZippedTrack
      -> [Cell String] 
      -> [[Cell String]]
sortedBacktrack board (_,[]) _ = board      
sortedBacktrack board track dictonary =
      sortedBacktrack newBoard newTrack dictonary
      where            
            (_,b) = track
            ((x,y), _) = head b            
            (newBoard, newStep) = setNextCanditate board (x,y) dictonary
            newTrack = newStep track

setNextCanditate :: [[Cell String]] -> (Int, Int) -> [Cell String] 
        -> ([[Cell String]], (ZippedTrack -> ZippedTrack))       
setNextCanditate board (x,y) dictonary
      | candidats == [] = ((replaceXY board (x,y) Empty), goBack)
      | otherwise        = ((replaceXY board (x,y) candidat), goForward)
      where 
            (_,z2s) = nextCell dictonary board (x,y)
            candidats  = [cell' | cell' <- z2s, (allowed board (x,y) cell')]
            candidat = head candidats  
            
nextCell :: [Cell String] -> [[Cell String]] -> (Int, Int)  
         -> ([Cell String],[Cell String])    
nextCell dictonary board (x,y) 
      | i ==  Nothing = splitAt 0 dictonary
      | otherwise     = splitAt (i'+1) dictonary
      where 
            cell = board `atXY` (x,y)
            i = findIndex (==cell) dictonary
            Just i' =  i             
            
--------------------------------------------------------------  
          
createSortedTrack :: [[Cell String]]  -> (Maybe Int, Maybe Int) -> [Cell String] 
      -> Track
      -- -> [((Int,Int), [Cell String])]
createSortedTrack _ (Nothing, Just _) _  = error "(Nothing, Just _)"    
createSortedTrack _ (Just _, Nothing) _  = error "(Just _, Nothing)"  
createSortedTrack board (ix,iy) dictonary
      | (mX,mY) == (Nothing,Nothing) = []
      | fix (atXY board (x,y)) == True = (createSortedTrack board (mX,mY) dictonary)
      | otherwise = 
            sortBy (\(_,a) (_,b) -> compare (length a) (length b)) 
                   ([(candidates  board (x,y) dictonary)] ++ (createSortedTrack board (mX,mY) dictonary))
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