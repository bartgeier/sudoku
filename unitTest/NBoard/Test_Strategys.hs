{-# LANGUAGE TemplateHaskell #-}
module Test_Strategys
(test_Strategys
) where

import PseudoMacros
import TestKit
import CreateSudoku
import Strategys

test_Strategys :: UnitTestState -> IO () 
test_Strategys this = do
      test_searchNakedSingles this
      test_searchHiddenSingles this


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
         
        

test_searchNakedSingles :: UnitTestState -> IO () 
test_searchNakedSingles this = do
      let Square{idx = i 
                ,row = r 
                ,column = c
                ,block = b
                ,owner = o
                ,candidates = cs } = create sudoku !! 70
          [[Square{candidates = cs'}]]  = searchNakedSingles (create sudoku)      
      tst_EQUAL this cs cs'
      putStrLn("searchNakedSingles, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      
test_searchHiddenSingles :: UnitTestState -> IO ()
test_searchHiddenSingles this = do
      let [[Square{candidates = cs'}]]  = searchHiddenSingles (create sudoku)      
      tst_EQUAL this ["8"] cs'
      putStrLn("searchNakedSingles, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  