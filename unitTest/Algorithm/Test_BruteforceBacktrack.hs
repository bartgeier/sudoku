{-# LANGUAGE TemplateHaskell #-}
module Test_BruteforceBacktrack
( test_BruteforceBacktrack
) where

import PseudoMacros
import TestKit
import Board
import BruteforceBacktrack
import BruteforceBacktrack.Internal


test_BruteforceBacktrack :: IO ()
test_BruteforceBacktrack = do
      test_nextCell
      test_setNextCanditate
      test_bruteforce


      
dictonary :: [Cell String]
dictonary = [Tmp "1",Tmp "2",Tmp "3",Tmp "4",Tmp "5",Tmp "6",Tmp "7",Tmp "8",Tmp "9"]

{-
sudoku_17 :: [[Cell String]]
sudoku_17 = (field ( " , , , , , , ,1, ," ++ "\n"
                  ++ "4, , , , , , , , ," ++ "\n"
                  ++ " ,2, , , , , , , ," ++ "\n"
                  ++ " , , , ,5, ,4, ,7," ++ "\n"
                  ++ " , ,8, , , ,3, , ," ++ "\n"
                  ++ " , ,1, ,9, , , , ," ++ "\n"
                  ++ "3, , ,4, , ,2, , ," ++ "\n"
                  ++ " ,5, ,1, , , , , ," ++ "\n"
                  ++ " , , ,8, ,6, , , ," ++ "\n")) 
                                  
solvedSudoku_17 :: [[Cell String]]
solvedSudoku_17 = (field ( "6,9,3,7,8,4,5,1,2," ++ "\n"
                        ++ "4,8,7,5,1,2,9,3,6," ++ "\n"
                        ++ "1,2,5,9,6,3,8,7,4," ++ "\n"
                        ++ "9,3,2,6,5,1,4,8,7," ++ "\n"
                        ++ "5,6,8,2,4,7,3,9,1," ++ "\n"
                        ++ "7,4,1,3,9,8,6,2,5," ++ "\n"
                        ++ "3,1,9,4,7,5,2,6,8," ++ "\n"
                        ++ "8,5,6,1,2,9,7,4,3," ++ "\n"
                        ++ "2,7,4,8,3,6,1,5,9," ++ "\n"))                   
-}

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

s0 :: [[Cell String]]               
s0 =           (field ( "5,3,4,6,7,8,9,1,2," ++ "\n"
                     ++ "6,7,2,1,9,5,3,4,8," ++ "\n"
                     ++ "1,9,8,3,4,2,5,6,7," ++ "\n"
                     ++ "8,5,9,7,6,1,4,2,3," ++ "\n"
                     ++ "4,2,6,8,5,3,7,9,1," ++ "\n"
                     ++ "7,1,3,9,2,4,8,5,6," ++ "\n"
                     ++ "9,6,1,5,3,7,2,8,4," ++ "\n"
                     ++ "2,8,7,4,1,9,6,3,5," ++ "\n"
                     ++ "3,4,5,2,8,6,1,7, ," ++ "\n"))  
                    
s1 :: [[Cell String]]               
s1 =           (field ( "5,3,4,6,7,8,9,1,2," ++ "\n"
                     ++ "6,7,2,1, ,5,3,4,8," ++ "\n"
                     ++ "1,9,8,3,4,2,5,6,7," ++ "\n"
                     ++ "8,5,9,7,6,1,4,2,3," ++ "\n"
                     ++ "4,2,6,8,5,3,7,9,1," ++ "\n"
                     ++ "7,1,3,9,2,4,8,5,6," ++ "\n"
                     ++ "9,6,1,5,3,7,2,8,4," ++ "\n"
                     ++ "2,8,7,4,1,9,6,3,5," ++ "\n"
                     ++ "3,4,5,2,8,6,1,7,9," ++ "\n"))      

s2 :: [[Cell String]]               
s2 =           (field ( "5,3,4,6,7,8,9,1,2," ++ "\n"
                     ++ "6,7,2,1,9,5,3,4,8," ++ "\n"
                     ++ "1,9,8,3,4,2,5,6,7," ++ "\n"
                     ++ "8,5,9,7,6,1,4,2,3," ++ "\n"
                     ++ "4,2,6,8,5,3,7,9,1," ++ "\n"
                     ++ "7,1,3,9,2,4,8,5,6," ++ "\n"
                     ++ "9,6,1,5,3,7,2,8,4," ++ "\n"
                     ++ "2,8,7,4,1,9,6,3,5," ++ "\n"
                     ++ " , ,5,2,8,6,1,7,9," ++ "\n"))       

s3 :: [[Cell String]]               
s3 =           (field ( "5,3,4,6,7,8,9,1,2," ++ "\n"
                     ++ "6,7,2,1,9,5,3,4,8," ++ "\n"
                     ++ "1,9,8,3,4,2,5,6,7," ++ "\n"
                     ++ "8,5,9,7,6,1,4,2,3," ++ "\n"
                     ++ "4,2,6,8, , , , ,1," ++ "\n"
                     ++ "7,1,3,9,2,4,8,5,6," ++ "\n"
                     ++ "9,6,1,5,3,7,2,8,4," ++ "\n"
                     ++ "2,8,7,4,1,9,6,3,5," ++ "\n"
                     ++ " , ,5,2,8,6,1,7,9," ++ "\n"))       
{-
s4 :: [[Cell String]]               
s4 =           (field ( "5,3,4,6,7,8,9,1,2," ++ "\n"
                     ++ " , , ,1,9,5, , , ," ++ "\n"
                     ++ " , ,8, , , , ,6, ," ++ "\n"
                     ++ "8, , , ,6, , , , ," ++ "\n"
                     ++ "4, , ,8, , , , ,1," ++ "\n"
                     ++ " , , , ,2, , , , ," ++ "\n"
                     ++ " ,6, , , , ,2,8, ," ++ "\n"
                     ++ " , , ,4,1,9, , ,5," ++ "\n"
                     ++ " , , , , , , ,7, ," ++ "\n"))   
                     
s5 :: [[Cell String]]               
s5 =           (field ( "5,3,4,6,7,8,9,1, ," ++ "\n"
                     ++ " , , ,1,9,5, , , ," ++ "\n"
                     ++ " , ,8, , , , ,6, ," ++ "\n"
                     ++ "8, , , ,6, , , , ," ++ "\n"
                     ++ "4, , ,8, , , , ,1," ++ "\n"
                     ++ " , , , ,2, , , , ," ++ "\n"
                     ++ " ,6, , , , ,2,8, ," ++ "\n"
                     ++ " , , ,4,1,9, , ,5," ++ "\n"
                     ++ " , , , , , , ,7, ," ++ "\n"))                        
-}
                        
               
           

board0 :: [[Cell String]]
board0 = (field ( "1,3, , , , , , , ," ++ "\n"
               ++ " , , ,1,9,5, , , ," ++ "\n"
               ++ " , ,8, , , , ,6, ," ++ "\n"
               ++ "8, , , ,6, , , , ," ++ "\n"
               ++ "4, , ,8, , , , ,1," ++ "\n"
               ++ " , , , ,2, , , , ," ++ "\n"
               ++ " ,6, , , , ,2,8, ," ++ "\n"
               ++ " , , ,4,1,9, , ,5," ++ "\n"
               ++ " , , , , , , ,7, ," ++ "\n"))                
               
board1 :: [[Cell String]]
board1 = (field ( "2,3, , , , , , , ," ++ "\n"
               ++ " , , ,1,9,5, , , ," ++ "\n"
               ++ " , ,8, , , , ,6, ," ++ "\n"
               ++ "8, , , ,6, , , , ," ++ "\n"
               ++ "4, , ,8, , , , ,1," ++ "\n"
               ++ " , , , ,2, , , , ," ++ "\n"
               ++ " ,6, , , , ,2,8, ," ++ "\n"
               ++ " , , ,4,1,9, , ,5," ++ "\n"
               ++ " , , , , , , ,7, ," ++ "\n"))     

board2 :: [[Cell String]]
board2 = (field ( "5,3, , , , , , , ," ++ "\n"
               ++ " , , ,1,9,5, , , ," ++ "\n"
               ++ " , ,8, , , , ,6, ," ++ "\n"
               ++ "8, , , ,6, , , , ," ++ "\n"
               ++ "4, , ,8, , , , ,1," ++ "\n"
               ++ " , , , ,2, , , , ," ++ "\n"
               ++ " ,6, , , , ,2,8, ," ++ "\n"
               ++ " , , ,4,1,9, , ,5," ++ "\n"
               ++ " , , , , , , ,7, ," ++ "\n"))                  

test_nextCell :: IO ()
test_nextCell = do
      tst_EQUAL 
            ([Tmp "1",Tmp "2",Tmp "3"], [Tmp "4",Tmp "5",Tmp "6",Tmp "7",Tmp "8",Tmp "9"]) 
            (nextCell dictonary sudoku (1, 0))--3
      putStrLn("nextCell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      
      tst_EQUAL 
            ([Tmp "1",Tmp "2",Tmp "3",Tmp "4",Tmp "5",Tmp "6",Tmp "7",Tmp "8",Tmp "9"],[]) 
            (nextCell dictonary sudoku (4, 1))--9
      putStrLn("nextCell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      
      tst_EQUAL 
            ([],[Tmp "1",Tmp "2",Tmp "3",Tmp "4",Tmp "5",Tmp "6",Tmp "7",Tmp "8",Tmp "9"]) 
            (nextCell dictonary sudoku (0, 0))--Empty
      putStrLn("nextCell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))                   
      
test_setNextCanditate :: IO ()
test_setNextCanditate = do  
      let (newBoard0, step0) = setNextCanditate sudoku (0,0) dictonary
      tst_EQUAL newBoard0 board0  
      tst_EQUAL (Just 1, Just 0) (step0 newBoard0 (Just 0, Just 0))
      putStrLn("setNextCanditate, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
 
      let (newBoard1, step1) = setNextCanditate board0 (0,0) dictonary
      tst_EQUAL newBoard1 board1  
      tst_EQUAL (Just 1, Just 0) (step1 newBoard1 (Just 0, Just 0))
      putStrLn("setNextCanditate, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      
      let (newBoard2, step2) = setNextCanditate board1 (0,0) dictonary
      tst_EQUAL newBoard2 board2  
      tst_EQUAL (Just 1, Just 0) (step2 newBoard2 (Just 0, Just 0))
      putStrLn("setNextCanditate, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
 
      let (newBoard10, step10) = setNextCanditate (replaceXY sudoku (0,0) (Tmp "9")) (0,0) dictonary
      tst_EQUAL newBoard10 sudoku 
      tst_EQUAL (Nothing, Nothing) (step10 newBoard10 (Just 0, Just 0))
      putStrLn("setNextCanditate, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      
      let (newBoard11, step11) = setNextCanditate (replaceXY sudoku (2,0) (Tmp "9")) (2,0) dictonary
      tst_EQUAL newBoard11 sudoku 
      tst_EQUAL (Just 1, Just 0) (step11 newBoard11 (Just 2, Just 0))
      putStrLn("setNextCanditate, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      


test_bruteforce :: IO ()
test_bruteforce = do  
      let s0' = bruteforce s0 (Nothing,Nothing) next dictonary
      tst_EQUAL s0' solvedSudoku
      putStrLn("bruteforce, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      
      let s1' = bruteforce s1 (Nothing,Nothing) next dictonary
      tst_EQUAL s1' solvedSudoku
      putStrLn("bruteforce, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   

      let s2' = bruteforce s2 (Nothing,Nothing) next dictonary
      tst_EQUAL s2' solvedSudoku
      putStrLn("bruteforce, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   

      let s3' = bruteforce s3 (Nothing,Nothing) next dictonary
      tst_EQUAL s3' solvedSudoku
      putStrLn("bruteforce, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      

{-
--view secounds
      let s4' = bruteforce s4 (Nothing,Nothing) next dictonary
      tst_EQUAL s4' solvedSudoku
      putStrLn("bruteforce, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  

      let s5' = bruteforce s5 (Nothing,Nothing) next dictonary
      tst_EQUAL s5' solvedSudoku
      putStrLn("bruteforce, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   
-}

{-
--25Min     
      let s' = bruteforce sudoku (Nothing,Nothing) next dictonary
      tst_EQUAL s' solvedSudoku
      putStrLn("bruteforce, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
-}      

{-
--1:45 Std
      let s17' = bruteforce sudoku_17 (Nothing,Nothing) next dictonary
      tst_EQUAL s17' solvedSudoku_17
      putStrLn("bruteforce, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
-}
    