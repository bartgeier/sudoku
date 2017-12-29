{-# LANGUAGE TemplateHaskell #-}
module Test_Board
( test_Board
) where

import PseudoMacros
import TestKit
import Board


test_Board :: IO ()
test_Board = do
      test_atXY
      test_back
      test_next
      test_replaceXY

      

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

test_atXY :: IO ()
test_atXY = do
      tst_EQUAL (Fix "3") (atXY sudoku (1, 0))
      putStrLn("atXY, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL True (fix (atXY sudoku (1, 0)))
      putStrLn("atXY, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   
      tst_EQUAL Empty (atXY sudoku (0, 0))
      putStrLn("atXY, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
               
test_back :: IO ()
test_back = do  
      tst_EQUAL (Just 8, Just 8) (back sudoku (Nothing,Nothing))
      putStrLn("back, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL (Nothing, Nothing) (back sudoku (Just 0, Just 0))
      putStrLn("back, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       
      tst_EQUAL (Just 8, Just 0) (back sudoku (Just 0, Just 1))
      putStrLn("back, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL (Just 0, Just 0) (back sudoku (Just 1, Just 0))
      putStrLn("back, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
      tst_EQUAL (Just 7, Just 8) (back sudoku (Just 8, Just 8))
      putStrLn("back, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL (Just 4, Just 5 ) (back sudoku (Just 5, Just 5))
      putStrLn("back, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL (Just 8, Just 4 ) (back sudoku (Just 0, Just 5))
      putStrLn("back, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
        
test_next :: IO ()
test_next = do  
      tst_EQUAL (Just 0, Just 0) (next sudoku (Nothing,Nothing))
      putStrLn("next, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL (Nothing, Nothing) (next sudoku (Just 8, Just 8))
      putStrLn("next, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       
      tst_EQUAL (Just 0, Just 2) (next sudoku (Just 8, Just 1))
      putStrLn("next, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL (Just 2, Just 0) (next sudoku (Just 1, Just 0))
      putStrLn("next, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
      tst_EQUAL (Just 1, Just 0) (next sudoku (Just 0, Just 0))
      putStrLn("next, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL (Just 6, Just 5 ) (next sudoku (Just 5, Just 5))
      putStrLn("next, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL (Just 0, Just 6 ) (next sudoku (Just 8, Just 5))
      putStrLn("next, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      

test_replaceXY :: IO ()    
test_replaceXY = do
      let board_0 = replaceXY sudoku (4,3) (Tmp "12")
      tst_EQUAL (Tmp "12") (atXY board_0 (4, 3))
      putStrLn("replaceXY, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      let board_1 = replaceXY sudoku (0,0) (Tmp "ABC")
      tst_EQUAL (Tmp "ABC") (atXY board_1 (0, 0))
      putStrLn("replaceXY, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   
      let board_2 = replaceXY sudoku (8,8) (Tmp "Hallo")
      tst_EQUAL (Tmp "Hallo") (atXY board_2 (8, 8))
      putStrLn("replaceXY, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))        