module Board
( Cell(Fix, Tmp, Empty)
, fix
, notFix
-- Field
, Field
, field
, grad
, endIndex
, isField
-- Block
, blocks
, blockIdx
-- Board
, atXY
, back
, next
, replaceXY
) where
 
import Control.Exception
import Tool_List
import Cell
import Field
import Block

{-
atXY :: Field -> (Maybe Int , Maybe Int) -> Cell String
atXY _ (Nothing, Nothing) = error "(Nothing,Nothing)"
atXY _ (Just _, Nothing) = error "(Just _, Nothing)"
atXY _ (Nothing, Just _) = error "(Nothing, Just _)"
atXY xs (Just x, Just y) = assert (isField xs) $
                           assert (y <= endIndex xs) $  
                           assert (x <= endIndex xs) $
                           (xs `at` y) `at` x
-}

atXY :: Field -> (Int , Int) -> Cell String
atXY xs (x, y) = assert (isField xs) $
                 assert (y <= endIndex xs) $  
                 assert (x <= endIndex xs) $
                (xs `at` y) `at` x                           

back :: Field -> (Maybe Int, Maybe Int) -> (Maybe Int, Maybe Int)
back board (Nothing, Nothing) = (Just (endIndex board), Just (endIndex board))
back _ (Just _, Nothing) = error "(Just _, Nothing)"
back _ (Nothing, Just _) = error "(Nothing, Just _)"
back _ (Just 0, Just 0) = (Nothing, Nothing)
back board (Just 0, Just y) = assert (isField board) $
                              assert (y <= endIndex board) $ 
                              (Just (endIndex board), Just (y-1))
back board (Just x, Just y) = assert (isField board) $
                              assert (y <= endIndex board) $  
                              assert (x <= endIndex board) $  
                              (Just (x-1), Just y)
                              
next :: Field -> (Maybe Int, Maybe Int) -> (Maybe Int, Maybe Int)
next _ (Nothing, Nothing) = (Just 0, Just 0)
next _   (Just _, Nothing) = error "(Just _, Nothing)"
next _ (Nothing, Just _) = error "(Nothing, Just _)"
next board (Just x, Just y) 
      | x == end && y == end = (Nothing, Nothing)
      | x == end && y < end  = (Just 0, Just (y+1))
      | x < end && y <= end  = (Just (x+1),Just y)
      | otherwise            = error "(x||y > endIndex)"
      where end = assert (isField board) $ endIndex board
     
replaceXY :: Field -> (Int, Int) -> Cell String -> Field
replaceXY board (x,y) cell = 
      let start = take y board
          work = board `at` y
          end = drop (y+1) board
          start' = take x work
          end' = drop (x+1) work
          work' = start' ++ [cell] ++ end'
      in  assert (isField board) $ start ++ [work'] ++ end



 