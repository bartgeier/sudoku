{-# LANGUAGE TemplateHaskell #-}
module Test_Rule
( test_Rule
) where

import PseudoMacros
import TestKit
import Rule
import Rule.Internal

import Field
import Cell



test_Rule :: IO ()
test_Rule = do
      test_row
      test_column
      test_blocka
      test_notAllowed
      test_allowed

      

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
               
test_row :: IO ()
test_row = do
      tst_EQUAL True (row sudoku 0 (Fix "3"))
      putStrLn("test_row, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL False (row sudoku 1 (Fix "3"))
      putStrLn("test_row, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL True (row sudoku 0 (Tmp "3"))
      putStrLn("test_row, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL False (row sudoku 1 (Tmp "3"))
      putStrLn("test_row, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
      tst_EQUAL False (row sudoku 2 (Tmp "3"))
      putStrLn("test_row, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL True (row sudoku (1) (Tmp "1"))
      putStrLn("test_row, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))     

test_column :: IO ()      
test_column = do 
      tst_EQUAL True (column sudoku 2 (Fix "8"))
      putStrLn("test_column, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL False (column sudoku 5 (Fix "8"))
      putStrLn("test_column, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL True (column sudoku 2 (Tmp "8"))
      putStrLn("test_column, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL False (column sudoku 4 (Tmp "8"))
      putStrLn("test_column, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
      tst_EQUAL False (column sudoku 2 (Tmp "3"))
      putStrLn("test_column, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL False (column sudoku (1) (Tmp "1"))
      putStrLn("test_column, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      
test_blocka :: IO ()    
test_blocka = do
      tst_EQUAL True (blockRule sudoku 0 0 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))     
      tst_EQUAL True (blockRule sudoku 1 0 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL True (blockRule sudoku 2 0 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL False (blockRule sudoku 3 0 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
            
      tst_EQUAL True (blockRule sudoku 0 1 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL True (blockRule sudoku 1 1 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL True (blockRule sudoku 2 1 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL False (blockRule sudoku 3 1 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       
      
      tst_EQUAL True (blockRule sudoku 0 2 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL True (blockRule sudoku 1 2 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL True (blockRule sudoku 2 2 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL False (blockRule sudoku 3 2 (Fix "3"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))        
      
      tst_EQUAL True (blockRule sudoku 3 4 (Fix "2"))
      putStrLn("blockRule, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  

test_notAllowed :: IO ()
test_notAllowed = do  
      tst_EQUAL True (notAllowed sudoku (0,0) (Tmp "3"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))     
      tst_EQUAL True (notAllowed sudoku (1,0) (Tmp "3"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL True (notAllowed sudoku (2,0) (Tmp "3"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       
      tst_EQUAL True (notAllowed sudoku (0,1) (Tmp "3"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))     
      tst_EQUAL True (notAllowed sudoku (1,1) (Tmp "3"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL True (notAllowed sudoku (2,1) (Tmp "3"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))           
      tst_EQUAL True (notAllowed sudoku (0,2) (Tmp "3"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))     
      tst_EQUAL True (notAllowed sudoku (1,2) (Tmp "3"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL True (notAllowed sudoku (2,2) (Tmp "3"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL False (notAllowed sudoku (3,3) (Tmp "5"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))     
      tst_EQUAL False (notAllowed sudoku (4,3) (Tmp "5"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL True (notAllowed sudoku (5,3) (Tmp "5"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       
      tst_EQUAL False (notAllowed sudoku (3,4) (Tmp "5"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))     
      tst_EQUAL False (notAllowed sudoku (4,4) (Tmp "5"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL True (notAllowed sudoku (5,4) (Tmp "5"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))           
      tst_EQUAL False (notAllowed sudoku (3,5) (Tmp "5"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))     
      tst_EQUAL False (notAllowed sudoku (4,5) (Tmp "5"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL True (notAllowed sudoku (5,5) (Tmp "5"))
      putStrLn("notAllowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))     

test_allowed :: IO ()
test_allowed = do  
      tst_EQUAL False (allowed sudoku (0,0) (Tmp "3"))
      putStrLn("allowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))        
      tst_EQUAL True (allowed sudoku (3,3) (Tmp "5"))
      putStrLn("allowed, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   