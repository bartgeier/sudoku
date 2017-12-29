{-# LANGUAGE TemplateHaskell #-}
module Test_Block
( test_Block
) where

import PseudoMacros
import TestKit

import Cell
import Block
import Block.Internal

test_Block :: IO ()
test_Block = do
      test_snippetRow
      test_snippets
      test_frstBlock
      test_blockRow
      test_glueToBlocks
      test_blocks
      test_blockIdx
  
test_snippetRow :: IO ()
test_snippetRow = do
      tst_EQUAL [[Tmp "11",Tmp "11"],[Tmp "12",Tmp "12"]]
                (snippetRow 2 [Tmp "11",Tmp "11",Tmp "12",Tmp "12"])
      putStrLn("snippetRow, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  

test_snippets :: IO ()      
test_snippets = do
      tst_EQUAL [[[Tmp "11",Tmp "11"],[Tmp "12",Tmp "12"]],[[Tmp "21",Tmp "21"],[Tmp "22",Tmp "22"]],[[Tmp "31",Tmp "32"],[Tmp "32",Tmp "32"]],[[Tmp "41",Tmp "42"],[Tmp "42",Tmp "42"]]] 
                (snippets 2 [[Tmp "11",Tmp "11",Tmp "12",Tmp "12"],[Tmp "21",Tmp "21",Tmp "22",Tmp "22"],[Tmp "31",Tmp "32",Tmp "32",Tmp "32"],[Tmp "41",Tmp "42",Tmp "42",Tmp "42"]])
      putStrLn("snippets, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       

test_frstBlock :: IO ()      
test_frstBlock = do
      tst_EQUAL ([Tmp "1",Tmp "1",Tmp "2",Tmp "2"],  [[Tmp "3",Tmp "3"],[Tmp "4",Tmp "4"]])                
                (frstBlock 2 [[Tmp "1",Tmp "1"],[Tmp "2",Tmp "2"],[Tmp "3",Tmp "3"],[Tmp "4",Tmp "4"]])
      putStrLn("frstBlock, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 

test_blockRow :: IO ()       
test_blockRow = do
      tst_EQUAL ([[Tmp "1",Tmp "1",Tmp "2",Tmp "2"], [Tmp "3",Tmp "3",Tmp "4",Tmp "4"]],  [])
                (blockRow 2 [[Tmp "1",Tmp "1"],[Tmp "2",Tmp "2"],[Tmp "3",Tmp "3"],[Tmp "4",Tmp "4"]])
      putStrLn("blockRow, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 

test_glueToBlocks :: IO ()      
test_glueToBlocks = do
      tst_EQUAL [[[Tmp "11",Tmp "11",Tmp "12",Tmp "12"],[Tmp "13",Tmp "13",Tmp "14",Tmp "14"]],  
                 [[Tmp "21",Tmp "21",Tmp "22",Tmp "22"],[Tmp "23",Tmp "23",Tmp "24",Tmp "24"]],
                 [[Tmp "31",Tmp "31",Tmp "32",Tmp "32"],[Tmp "33",Tmp "33",Tmp "34",Tmp "34"]],
                 [[Tmp "41",Tmp "41",Tmp "42",Tmp "42"],[Tmp "43",Tmp "43",Tmp "44",Tmp "44"]]]
                               
                (glueToBlocks 2 [[[Tmp "11",Tmp "11"],[Tmp "12",Tmp "12"],[Tmp "13",Tmp "13"],[Tmp "14",Tmp "14"]],  
                               [[Tmp "21",Tmp "21"],[Tmp "22",Tmp "22"],[Tmp "23",Tmp "23"],[Tmp "24",Tmp "24"]],
                               [[Tmp "31",Tmp "31"],[Tmp "32",Tmp "32"],[Tmp "33",Tmp "33"],[Tmp "34",Tmp "34"]],
                               [[Tmp "41",Tmp "41"],[Tmp "42",Tmp "42"],[Tmp "43",Tmp "43"],[Tmp "44",Tmp "44"]]])
      putStrLn("glueToBlocks, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 

test_blocks :: IO ()      
test_blocks = do
      tst_EQUAL [[[Tmp "11",Tmp "12",Tmp "13",Tmp "21",Tmp "22",Tmp "23",Tmp "31",Tmp "32",Tmp "33"], [Tmp "14",Tmp "15",Tmp "16",Tmp "24",Tmp "25",Tmp "26",Tmp "34",Tmp "35",Tmp "36"], [Tmp "17",Tmp "18",Tmp "19",Tmp "27",Tmp "28",Tmp "29",Tmp "37",Tmp "38",Tmp "39"]],
                 [[Tmp "41",Tmp "42",Tmp "43",Tmp "51",Tmp "52",Tmp "53",Tmp "61",Tmp "62",Tmp "63"], [Tmp "44",Tmp "45",Tmp "46",Tmp "54",Tmp "55",Tmp "56",Tmp "64",Tmp "65",Tmp "66"], [Tmp "47",Tmp "48",Tmp "49",Tmp "57",Tmp "58",Tmp "59",Tmp "67",Tmp "68",Tmp "69"]],
                 [[Tmp "71",Tmp "72",Tmp "73",Tmp "81",Tmp "82",Tmp "83",Tmp "91",Tmp "92",Tmp "93"], [Tmp "74",Tmp "75",Tmp "76",Tmp "84",Tmp "85",Tmp "86",Tmp "94",Tmp "95",Tmp "96"], [Tmp "77",Tmp "78",Tmp "79",Tmp "87",Tmp "88",Tmp "89",Tmp "97",Tmp "98",Tmp "99"]]]
                                                                                                                                                                           
                 (blocks [[Tmp "11",Tmp "12",Tmp "13",Tmp "14",Tmp "15",Tmp "16",Tmp "17",Tmp "18",Tmp "19"],
                         [Tmp "21",Tmp "22",Tmp "23",Tmp "24",Tmp "25",Tmp "26",Tmp "27",Tmp "28",Tmp "29"],
                         [Tmp "31",Tmp "32",Tmp "33",Tmp "34",Tmp "35",Tmp "36",Tmp "37",Tmp "38",Tmp "39"],
                         [Tmp "41",Tmp "42",Tmp "43",Tmp "44",Tmp "45",Tmp "46",Tmp "47",Tmp "48",Tmp "49"],
                         [Tmp "51",Tmp "52",Tmp "53",Tmp "54",Tmp "55",Tmp "56",Tmp "57",Tmp "58",Tmp "59"],
                         [Tmp "61",Tmp "62",Tmp "63",Tmp "64",Tmp "65",Tmp "66",Tmp "67",Tmp "68",Tmp "69"],
                         [Tmp "71",Tmp "72",Tmp "73",Tmp "74",Tmp "75",Tmp "76",Tmp "77",Tmp "78",Tmp "79"],
                         [Tmp "81",Tmp "82",Tmp "83",Tmp "84",Tmp "85",Tmp "86",Tmp "87",Tmp "88",Tmp "89"],
                         [Tmp "91",Tmp "92",Tmp "93",Tmp "94",Tmp "95",Tmp "96",Tmp "97",Tmp "98",Tmp "99"]])                        
      putStrLn("blocks, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      
test_blockIdx :: IO ()      
test_blockIdx = do      
      tst_EQUAL 0 (blockIdx 3 0)
      putStrLn("blockIdx, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL 0 (blockIdx 3 1)
      putStrLn("blockIdx, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL 0 (blockIdx 3 2)
      putStrLn("blockIdx, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL 1 (blockIdx 3 3)
      putStrLn("blockIdx, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL 1 (blockIdx 3 4)
      putStrLn("blockIdx, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL 1 (blockIdx 3 5)
      putStrLn("blockIdx, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   
      tst_EQUAL 2 (blockIdx 3 6)
      putStrLn("blockIdx, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL 2 (blockIdx 3 7)
      putStrLn("blockIdx, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL 2 (blockIdx 3 8)
      putStrLn("blockIdx, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))        