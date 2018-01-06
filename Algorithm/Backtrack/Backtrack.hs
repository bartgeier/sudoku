module Backtrack
( backtrack
, setNextCanditate
, reducing
) where

import Board
import Data.List
import Rule
import Tool_List
import Track
         
backtrack :: [[Cell String]] -> ZippedTrack -> [Cell String] 
      -> [[Cell String]]
backtrack board (_,[]) _ = board      
backtrack board track dictonary =
      backtrack newBoard newTrack dictonary
      where            
            (_,b) = track
            (x,y) = head b            
            (newBoard, step) = setNextCanditate board (x,y) dictonary
            newTrack = step track

setNextCanditate :: [[Cell String]] -> (Int, Int) -> [Cell String] 
        -> ([[Cell String]], (ZippedTrack -> ZippedTrack))       
setNextCanditate board (x,y) dictonary
      | candidats == [] = ((replaceXY board (x,y) Empty), goBack)
      | otherwise       = ((replaceXY board (x,y) candidat), goForward)
      where 
            reduceds = reducing dictonary board (x,y)
            candidats  = [r | r <- reduceds, (allowed board (x,y) r)]
            candidat = head candidats  
            
reducing :: [Cell String] -> [[Cell String]] -> (Int, Int)  
         -> [Cell String]
reducing dictonary board (x,y) 
      | i ==  Nothing = snd (splitAt 0 dictonary)
      | otherwise     = snd (splitAt (i'+1) dictonary)
      where 
            cell = board `atXY` (x,y)
            i = findIndex (==cell) dictonary
            Just i' =  i             
            