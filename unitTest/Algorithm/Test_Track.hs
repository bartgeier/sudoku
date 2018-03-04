{-# LANGUAGE TemplateHaskell #-}
module Test_Track
( test_Track
) where

import PseudoMacros
import TestKit
import Board
import Track
import Track.Internal
import Tool_List

test_Track :: UnitTestState -> IO ()
test_Track this = do
      test_candidates this 
      test_createSorted this 
      test_goForward this 
      test_goBack this 
      
dictonary :: [Cell String]
dictonary = [Tmp "1",Tmp "2",Tmp "3",Tmp "4",Tmp "5",Tmp "6",Tmp "7",Tmp "8",Tmp "9"]
                      
sudoku :: Field
sudoku = (field ( " ,3, , , , , , , ," ++ "\n"
               ++ " , , ,1,9,5, , , ," ++ "\n"
               ++ " , ,8, , , , ,6, ," ++ "\n"
               ++ "8, , , ,6, , , , ," ++ "\n"
               ++ "4, , ,8, , , , ,1," ++ "\n"
               ++ " , , , ,2, , , , ," ++ "\n"
               ++ " ,6, , , , ,2,8, ," ++ "\n"
               ++ " , , ,4,1,9, , ,5," ++ "\n"
               ++ " , , , , , , ,7, ," ++ "\n")) 
                 
s0 :: Field               
s0 = (field ( "5,3,4,6,7,8,9,1,2," ++ "\n"
           ++ "6,7,2,1,9,5,3,4,8," ++ "\n"
           ++ "1,9,8,3,4,2,5,6,7," ++ "\n"
           ++ "8,5,9,7,6,1,4,2,3," ++ "\n"
           ++ "4,2,6,8,5,3,7,9,1," ++ "\n"
           ++ "7,1,3,9,2,4,8,5,6," ++ "\n"
           ++ "9,6,1,5,3,7,2,8,4," ++ "\n"
           ++ "2,8,7,4,1,9,6,3,5," ++ "\n"
           ++ "3,4,5,2,8,6,1,7, ," ++ "\n")) 
                                     
test_candidates :: UnitTestState -> IO ()         
test_candidates this = do
      tst_EQUAL this ((0,2),[Tmp "1",Tmp "2",Tmp "5",Tmp "7",Tmp "9"]) (candidates sudoku (0,2) dictonary)
      putStrLn("candidates, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      
      tst_EQUAL this ((1,0),[]) (candidates sudoku (1,0) dictonary)
      putStrLn("candidates, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))

test_createSorted :: UnitTestState -> IO ()
test_createSorted this = do
      let track1 = Track.createSorted sudoku dictonary
          
      tst_EQUAL this 61 (length track1)
      putStrLn("createSorted, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      
      tst_EQUAL this ((7,7)) (head track1)
      putStrLn("createSorted, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 

      tst_EQUAL this ((6,5)) (last track1)
      putStrLn("createSorted, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 

test_goForward :: UnitTestState -> IO ()
test_goForward this = do
      let track10 = Track.createSorted s0 dictonary
      tst_EQUAL this 1 (length track10)
      putStrLn("goForward, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL this [(8,8)] track10
      putStrLn("goForward, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      
      let zippedTrack = goForward ([],track10)
      tst_EQUAL this ([(8,8)],[]) zippedTrack
      putStrLn("goForward, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      
test_goBack :: UnitTestState -> IO ()
test_goBack this = do
      let track20 = Track.createSorted s0 dictonary
      tst_EQUAL this 1 (length track20)
      putStrLn("goBack, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))             
      tst_EQUAL this [(8,8)] track20
      putStrLn("goBack, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      
      let zippedTrack = goBack (track20,[])
      tst_EQUAL this ([],[(8,8)]) zippedTrack
      putStrLn("goBack, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
