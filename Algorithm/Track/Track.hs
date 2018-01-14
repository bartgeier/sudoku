module Track
( Track
, ZippedTrack
, createSorted
) where

import Board
import Track.Internal
 
type Track  = [(Int,Int)] 
type ZippedTrack  = (Track ,Track)

createSorted :: Field -> [Cell String] 
      -> Track
createSorted board dictonary =
      [x | (x,_) <- sorted board (Nothing,Nothing) dictonary] 

{-      
sorted :: [[Cell String]]  -> (Maybe Int, Maybe Int) -> [Cell String] 
      -> [ ((Int,Int), [Cell String]) ]
sorted _ (Nothing, Just _) _  = error "(Nothing, Just _)"    
sorted _ (Just _, Nothing) _  = error "(Just _, Nothing)"  
sorted board (ix,iy) dictonary
      | (mX,mY) == (Nothing,Nothing) = []
      | fix (atXY board (x,y)) == True = (sorted board (mX,mY) dictonary)
      | otherwise = 
            sortBy (\(_,a) (_,b) -> compare (length a) (length b)) 
                   ([(candidates  board (x,y) dictonary)] ++ (sorted board (mX,mY) dictonary))
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
-}
