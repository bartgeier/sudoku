{-# LANGUAGE TemplateHaskell #-}
module Test_Tool_List
( test_Tool_List
) where

import PseudoMacros
import TestKit
import Tool_List

test_Tool_List :: UnitTestState -> IO ()
test_Tool_List this = do
      test_at this
      test_goForward this
      test_goBack this

test_at :: UnitTestState -> IO ()          
test_at this = do
      let xs = [0,1,2,3,4]::[Int]
          x_0 = xs `at` 0
          x_1 = xs `at` 1
          x_2 = xs `at` 2
          x_3 = at xs 3
          x_4 = at xs 4
      tst_EQUAL this 0 x_0
      putStrLn("at, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this 1 x_1
      putStrLn("at, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this 2 x_2
      putStrLn("at, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this 3 x_3
      putStrLn("at, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this 4 x_4
      putStrLn("at, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))

               
test_goForward :: UnitTestState -> IO ()          
test_goForward this = do
      let xs = [1,2,3,4]::[Int]
          xs_1 = goForward ([],xs)
          xs_2 = goForward (xs_1)
          xs_3 = goForward (xs_2)
          xs_4 = goForward (xs_3)
          xs_5 = goForward (xs_4)
      tst_EQUAL this ([1],[2,3,4]) xs_1
      putStrLn("goForward, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this ([1,2],[3,4]) xs_2
      putStrLn("goForward, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this ([1,2,3],[4]) xs_3
      putStrLn("goForward, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this ([1,2,3,4],[]) xs_4
      putStrLn("goForward, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this ([1,2,3,4],[]) xs_5
      putStrLn("goForward, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))

test_goBack :: UnitTestState -> IO ()          
test_goBack this = do
      let xs = [1,2,3,4]::[Int]
          xs_1 = goBack (xs,[])
          xs_2 = goBack (xs_1)
          xs_3 = goBack (xs_2)
          xs_4 = goBack (xs_3)
          xs_5 = goBack (xs_4)
      tst_EQUAL this ([1,2,3],[4]) xs_1
      putStrLn("goBack, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this ([1,2],[3,4]) xs_2
      putStrLn("goBack, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this ([1],[2,3,4]) xs_3
      putStrLn("goBack, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this ([],[1,2,3,4]) xs_4
      putStrLn("goBack, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this ([],[1,2,3,4]) xs_5
      putStrLn("goBack, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))                  