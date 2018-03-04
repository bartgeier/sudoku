module TestKit
( tst_EQUAL
, tst_TOTAL
, UnitTestState
) where

--{-# LANGUAGE TemplateHaskell #-}
--import PseudoMacros
import System.Console.ANSI

import Data.IORef


type UnitTestState = IORef (Int,Int)

tst_EQUAL :: (Eq a, Show a) => UnitTestState -> a -> a -> IO ()       
tst_EQUAL this x y = do
      (numOfTest,numOfFail) <- readIORef this
      putStr $ show numOfTest ++ " "
      let c' = numOfTest+1
      if x == y
            then  do setSGR [Reset ]
                     putStr $ "OK" ++ "   => "
                     writeIORef this (c',numOfFail)
            else do setSGR [SetColor Foreground Vivid Red]
                    putStr $ "fail" ++ " => " ++ show x ++ " /= " ++ show y ++ " "
                    writeIORef this (c',(numOfFail+1))
      setSGR [Reset ]
                    
tst_TOTAL :: UnitTestState -> IO ()       
tst_TOTAL this = do
      (numOfTest,numOfFail) <- readIORef this
      if numOfFail == 0
            then  do setSGR [SetColor Foreground Vivid Green]
                     putStr $ "All " ++ show numOfTest ++ " Tests OK"
            else do setSGR [SetColor Foreground Vivid Red]
                    putStr $ show numOfFail ++ " of " ++ show numOfTest ++ " Tests failed"
      setSGR [Reset ]
                    
-- cabal install ansi-terminal            

