module TestKit
( tst_EQUAL
) where

{-# LANGUAGE TemplateHaskell #-}
import PseudoMacros


tst_EQUAL :: (Eq a, Show a) => a -> a -> IO ()       
tst_EQUAL x y = do
      if x == y
            then putStr $ "OK" ++ "   => "
            else putStr $ "fail" ++ " => " ++ show x ++ " /= " ++ show y ++ " "

            

