{-# LANGUAGE TemplateHaskell #-}
module Test_CreateSudoku
( test_CreateSudoku
) where

import PseudoMacros
import TestKit
import CreateSudoku

test_CreateSudoku :: UnitTestState -> IO () 
test_CreateSudoku this = do
      test_canditates this
      test_cell_1 this
      test_cell_20 this
      test_cell_70 this

sudoku :: String
sudoku =  ( " ,3, , , , , , , ," ++ "\n"
         ++ " , , ,1,9,5, , , ," ++ "\n"
         ++ " , ,8, , , , ,6, ," ++ "\n"
         ++ "8, , , ,6, , , , ," ++ "\n"
         ++ "4, , ,8, , , , ,1," ++ "\n"
         ++ " , , , ,2, , , , ," ++ "\n"
         ++ " ,6, , , , ,2,8, ," ++ "\n"
         ++ " , , ,4,1,9, , ,5," ++ "\n"
         ++ " , , , , , , ,7, ," ++ "\n")

test_canditates :: UnitTestState -> IO () 
test_canditates this = do
      tst_EQUAL this ["3","1","9","5","8","6","4","2","7"] (canditates sudoku)
      putStrLn("canditates, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
    
test_cell_1 :: UnitTestState -> IO ()
test_cell_1 this = do 
      let Square{idx = i 
                ,row = r 
                ,column = c
                ,block = b
                ,owner = o
                ,candidates = cs } = create sudoku !! 1 
      tst_EQUAL this i 1      
      putStrLn("cell_1, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL this r 0      
      putStrLn("cell_1, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   
      tst_EQUAL this c 1      
      putStrLn("cell_1, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this b 0      
      putStrLn("cell_1, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       
      tst_EQUAL this o "3"
      putStrLn("cell_1, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL this cs ["1","9","5","4","2","7"]
      putStrLn("cell_1, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       

test_cell_20 :: UnitTestState -> IO ()
test_cell_20 this = do 
      let Square{idx = i 
                ,row = r 
                ,column = c
                ,block = b
                ,owner = o
                ,candidates = cs } = create sudoku !! 20  
      tst_EQUAL this i 20      
      putStrLn("cell_20, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL this r 2      
      putStrLn("cell_20, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   
      tst_EQUAL this c 2      
      putStrLn("cell_20, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
      tst_EQUAL this b 0      
      putStrLn("cell_20, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL this o "8"
      putStrLn("cell_20, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL this cs ["1","9","5","4","2","7"]
      putStrLn("cell_20, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   

test_cell_70 :: UnitTestState -> IO ()
test_cell_70 this = do 
      let Square{idx = i 
                ,row = r 
                ,column = c
                ,block = b
                ,owner = o
                ,candidates = cs } = create sudoku !! 70  
      tst_EQUAL this i 70     
      putStrLn("cell_70, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL this r 7      
      putStrLn("cell_70, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   
      tst_EQUAL this c 7      
      putStrLn("cell_70, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
      tst_EQUAL this b 8      
      putStrLn("cell_70, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL this o ""
      putStrLn("cell_70, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL this cs ["3"]
      putStrLn("cell_70, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))         
   