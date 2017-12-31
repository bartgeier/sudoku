{-# LANGUAGE TemplateHaskell #-}
module Test_SortedBacktrack
( test_SortedBacktrack
) where

import PseudoMacros
import TestKit
import Board
import SortedBacktrack
import qualified Data.Map as Map



test_SortedBacktrack :: IO ()
test_SortedBacktrack = do
      test_candidates
      test_track


      
dictonary :: [Cell String]
dictonary = [Tmp "1",Tmp "2",Tmp "3",Tmp "4",Tmp "5",Tmp "6",Tmp "7",Tmp "8",Tmp "9"]

sudoku :: [[Cell String]]
sudoku = (field ( " ,3, , , , , , , ," ++ "\n"
               ++ " , , ,1,9,5, , , ," ++ "\n"
               ++ " , ,8, , , , ,6, ," ++ "\n"
               ++ "8, , , ,6, , , , ," ++ "\n"
               ++ "4, , ,8, , , , ,1," ++ "\n"
               ++ " , , , ,2, , , , ," ++ "\n"
               ++ " ,6, , , , ,2,8, ," ++ "\n"
               ++ " , , ,4,1,9, , ,5," ++ "\n"
               ++ " , , , , , , ,7, ," ++ "\n")) 
               
solvedSudoku :: [[Cell String]]               
solvedSudoku = (field ( "5,3,4,6,7,8,9,1,2," ++ "\n"
                     ++ "6,7,2,1,9,5,3,4,8," ++ "\n"
                     ++ "1,9,8,3,4,2,5,6,7," ++ "\n"
                     ++ "8,5,9,7,6,1,4,2,3," ++ "\n"
                     ++ "4,2,6,8,5,3,7,9,1," ++ "\n"
                     ++ "7,1,3,9,2,4,8,5,6," ++ "\n"
                     ++ "9,6,1,5,3,7,2,8,4," ++ "\n"
                     ++ "2,8,7,4,1,9,6,3,5," ++ "\n"
                     ++ "3,4,5,2,8,6,1,7,9," ++ "\n"))                

          
test_candidates :: IO ()          
test_candidates = do
      tst_EQUAL ((0,2),[Tmp "1",Tmp "2",Tmp "5",Tmp "7",Tmp "9"]) (candidates sudoku (0,2) dictonary)
      putStrLn("candidates, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      
      tst_EQUAL ((1,0),[]) (candidates sudoku (1,0) dictonary)
      putStrLn("candidates, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))


test_track :: IO ()
test_track = do
      let t = track sudoku (Nothing,Nothing) dictonary
          mapT = Map.fromList t
          
      tst_EQUAL 61 (length t)
      putStrLn("track, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      
      tst_EQUAL ((7,7),[Tmp "3"]) (head t)
      putStrLn("track, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      
      tst_EQUAL ((7,7),[Tmp "3"]) (head t)
      putStrLn("track, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      
      let (Just a0) = (Map.lookup (5,4) mapT)
      tst_EQUAL [Tmp "3",Tmp "7"] a0
      putStrLn("track, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      
      let a1 = (Map.lookup (1,0) mapT)
      tst_EQUAL Nothing a1
      putStrLn("track, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      
      let (Just a2) = (Map.lookup (6,5) mapT)
      tst_EQUAL [Tmp "3",Tmp "4",Tmp "5",Tmp "6",Tmp "7",Tmp "8",Tmp "9"] a2
      putStrLn("track, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))

 


    