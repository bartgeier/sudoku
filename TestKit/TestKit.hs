module TestKit
( tst_EQUAL
) where

--{-# LANGUAGE TemplateHaskell #-}
--import PseudoMacros
import System.Console.ANSI


-- cabal install ansi-terminal

tst_EQUAL :: (Eq a, Show a) => a -> a -> IO ()       
tst_EQUAL x y =
      if x == y
            then  do setSGR [Reset ]
                     putStr $ "OK" ++ "   => "
            else do setSGR [SetColor Foreground Vivid Red]
                    putStr $ "fail" ++ " => " ++ show x ++ " /= " ++ show y ++ " "

            

