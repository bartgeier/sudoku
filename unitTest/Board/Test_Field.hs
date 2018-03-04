{-# LANGUAGE TemplateHaskell #-}
module Test_Field
( test_Field
) where

import PseudoMacros
import TestKit

import Field
import Field.Internal
import Cell

test_Field :: UnitTestState -> IO ()
test_Field this = do
      test_createRow this       
      test_field this 
      test_isSquare this     
      test_checkWidths this 
      test_isLineSquare this 
      test_width this 
      test_height this 
      test_grad this 
      test_endIndex this 
      
      
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
         
test_createRow :: UnitTestState -> IO ()
test_createRow this = do
      tst_EQUAL this [Fix "0",Fix "1",Fix "2",Fix "3",Empty,Fix "5",Fix "6",Fix "7",Fix "8",Fix "9"] 
                (createRow "0,1,2,3, ,5,6,7,8,9")
      putStrLn("createRow, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))

test_field :: UnitTestState -> IO ()
test_field this = do
      tst_EQUAL this [[Fix "1", Empty, Fix "2"],[Fix "9", Fix "4", Empty]] 
                (field "1,,2\n9,4, ,")
      putStrLn("field 0, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this [[Fix "1", Empty, Fix "2"],[Fix "9", Fix "4", Empty]] 
                (field " 1, , 2 ,\n 9,4 ,,")
      putStrLn("field 1, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL this [[Empty, Fix "1", Empty, Fix "2"],[Fix "9", Fix "4", Empty]] 
                (field ",1, , 2,\n9,4 ,,")
      putStrLn("field 2, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       

test_isSquare :: UnitTestState -> IO ()     
test_isSquare this = do      
      tst_EQUAL this False 
                (isSquare [[Empty,Empty,Empty,Empty],[]])
      putStrLn("isSquare 0, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))     
      tst_EQUAL this True 
                (isSquare [[Empty,Empty],[Empty,Empty]])
      putStrLn("isSquare 1, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  

test_checkWidths :: UnitTestState -> IO ()     
test_checkWidths this = do
      tst_EQUAL this (Just 0) 
                (checkWidths [[Empty,Empty],[Empty],[Empty,Empty]])
      putStrLn("checkWidths 0, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this (Just 0) 
                (checkWidths [[Empty,Empty],[Empty, Empty, Empty],[Empty,Empty]])
      putStrLn("checkWidths 1, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL this (Just 2) 
                (checkWidths [[Empty,Empty],[Empty, Empty],[Empty,Empty]])
      putStrLn("checkWidths 2, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL this (Just 0) 
                (checkWidths [[]])
      putStrLn("checkWidths 3, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL this (Just 4) 
                (checkWidths [[Empty,Empty,Empty,Empty]])
      putStrLn("checkWidths 4, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   
      tst_EQUAL this (Just 0) 
                (checkWidths [[Empty,Empty,Empty,Empty],[]])
      putStrLn("checkWidths 5, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))             

test_isLineSquare :: UnitTestState -> IO ()      
test_isLineSquare this = do 
      tst_EQUAL this False 
                (isLineSquare [[Empty, Empty, Empty]])
      putStrLn("isLineSquare 0, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   
      tst_EQUAL this True 
                (isLineSquare [[Empty, Empty, Empty, Empty]])
      putStrLn("isLineSquare 1, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 

test_width :: UnitTestState -> IO ()      
test_width this = do
      tst_EQUAL this 4 
                  (width [[Tmp "11",Tmp "12",Tmp "13",Tmp "14"],
                          [Tmp "21",Tmp "22",Tmp "23",Tmp "24"],
                          [Tmp "31",Tmp "32",Tmp "33",Tmp "34"],
                          [Tmp "41",Tmp "42",Tmp "43",Tmp "44"]])
      putStrLn("test_width, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  

test_height :: UnitTestState -> IO ()      
test_height this = do
      tst_EQUAL this 4 
                  (height [[Tmp "11",Tmp "12",Tmp "13",Tmp "14"],
                          [Tmp "21",Tmp "22",Tmp "23",Tmp "24"],
                          [Tmp "31",Tmp "32",Tmp "33",Tmp "34"],
                          [Tmp "41",Tmp "42",Tmp "43",Tmp "44"]])
      putStrLn("test_height, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      

test_grad :: UnitTestState -> IO ()     
test_grad this = do
      tst_EQUAL this 3 (grad [[Tmp "11",Tmp "12",Tmp "13",Tmp "14",Tmp "15",Tmp "16",Tmp "17",Tmp "18",Tmp "19"],
                         [Tmp "21",Tmp "22",Tmp "23",Tmp "24",Tmp "25",Tmp "26",Tmp "27",Tmp "28",Tmp "29"],
                         [Tmp "31",Tmp "32",Tmp "33",Tmp "34",Tmp "35",Tmp "36",Tmp "37",Tmp "38",Tmp "39"],
                         [Tmp "41",Tmp "42",Tmp "43",Tmp "44",Tmp "45",Tmp "46",Tmp "47",Tmp "48",Tmp "49"],
                         [Tmp "51",Tmp "52",Tmp "53",Tmp "54",Tmp "55",Tmp "56",Tmp "57",Tmp "58",Tmp "59"],
                         [Tmp "61",Tmp "62",Tmp "63",Tmp "64",Tmp "65",Tmp "66",Tmp "67",Tmp "68",Tmp "69"],
                         [Tmp "71",Tmp "72",Tmp "73",Tmp "74",Tmp "75",Tmp "76",Tmp "77",Tmp "78",Tmp "79"],
                         [Tmp "81",Tmp "82",Tmp "83",Tmp "84",Tmp "85",Tmp "86",Tmp "87",Tmp "88",Tmp "89"],
                         [Tmp "91",Tmp "92",Tmp "93",Tmp "94",Tmp "95",Tmp "96",Tmp "97",Tmp "98",Tmp "99"]])
      putStrLn("test_grad, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL this 2 (grad [[Tmp "11",Tmp "12",Tmp "13",Tmp "14"],
                         [Tmp "21",Tmp "22",Tmp "23",Tmp "24"],
                         [Tmp "31",Tmp "32",Tmp "33",Tmp "34"],
                         [Tmp "41",Tmp "42",Tmp "43",Tmp "44"]])
      putStrLn("test_grad, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       
      {-tst_EQUAL 2 (grad [[Tmp "11",Tmp "12",Tmp "13",Tmp "14"],
                         [Tmp "21",Tmp "22",Tmp "23",Tmp "24"]])
      putStrLn("test_grad, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       -}

test_endIndex :: UnitTestState -> IO ()      
test_endIndex this = do
      tst_EQUAL this 8 (endIndex sudoku)
      putStrLn("endIndex, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
  
     