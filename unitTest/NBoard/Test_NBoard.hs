{-# LANGUAGE TemplateHaskell #-}
module Test_NBoard
( test_NBoard
) where

import PseudoMacros
import TestKit
import NBoard


test_NBoard :: UnitTestState -> IO () 
test_NBoard this = do
      test_canditates this
      test_emptyBoard this


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
    
test_emptyBoard :: UnitTestState -> IO ()
test_emptyBoard this = do 
      putStrLn (show (numbering 0 (emptyBoard sudoku)))
      putStrLn("canditates, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      putStrLn (show (puzzel sudoku))
      putStrLn (show (create sudoku))