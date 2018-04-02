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
import Test_Track
import Test_Backtrack
import Test_NBoard

import Data.IORef

import TestKit

                      
main :: IO ()
main = do 
      ref <- newIORef (0::Int,0::Int)
      test_Tool_List ref
      putStrLn "  "
      test_cell ref
      putStrLn "  "
      test_Field ref
      putStrLn "  "
      test_Block ref     
      putStrLn "  "     
      test_Rule ref
      putStrLn "  "
      test_Board ref
      putStrLn "  "
      test_Track ref
      putStrLn "  "
      test_Backtrack ref
      test_NBoard ref
      tst_TOTAL ref
      
