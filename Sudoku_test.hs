--{-# LANGUAGE TemplateHaskell #-}
module Main where
--import PseudoMacros
--import TestKit
import Test_Tool_List
import Test_Cell
import Test_Field
import Test_Block
import Test_Rule
import Test_Board
import Test_BruteforceBacktrack
import Test_Track
import Test_Backtrack

main :: IO ()
main = do 
      test_Tool_List
      putStrLn "  "
      test_cell
      putStrLn "  "
      test_Field
      putStrLn "  "
      test_Block      
      putStrLn "  "     
      test_Rule
      putStrLn "  "
      test_Board
      putStrLn "  "
      test_BruteforceBacktrack
      putStrLn "  "
      test_Track
      putStrLn "  "
      test_Backtrack
      
      
