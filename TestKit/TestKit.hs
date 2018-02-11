module TestKit
( tst_EQUAL
, tst_EQUAL_C
, Counter
, makeCounter
) where

--{-# LANGUAGE TemplateHaskell #-}
--import PseudoMacros
import System.Console.ANSI

import Data.IORef

type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do
      r <- newIORef 0
      return (\i -> do modifyIORef r (+i)
                       readIORef r)
                       
                       
      
{-      counter <- makeCounter
      counter 1
      c <- counter 3
      d <- counter 1
      print [c,d]                       
      -}

tst_EQUAL :: (Eq a, Show a) => a -> a -> IO ()       
tst_EQUAL x y =
      if x == y
            then  do setSGR [Reset ]
                     putStr $ "OK" ++ "   => "
            else do setSGR [SetColor Foreground Vivid Red]
                    putStr $ "fail" ++ " => " ++ show x ++ " /= " ++ show y ++ " "

tst_EQUAL_C :: (Eq a, Show a) => Counter -> a -> a -> IO ()       
tst_EQUAL_C counter x y = do
      c <- counter 1
      putStr $ show c ++ " "
      if x == y
            then  do setSGR [Reset ]
                     putStr $ "OK" ++ "   => "
            else do setSGR [SetColor Foreground Vivid Red]
                    putStr $ "fail" ++ " => " ++ show x ++ " /= " ++ show y ++ " "
                    
                    
-- cabal install ansi-terminal            

