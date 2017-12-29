--{-# LANGUAGE TemplateHaskell #-}
module Main where
--import PseudoMacros
--import TestKit
import Test_Cell
import Test_Field
import Test_Block
import Test_Rule
import Test_Board
import Test_BruteforceBacktrack

main :: IO ()
main = do 
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
      
      
