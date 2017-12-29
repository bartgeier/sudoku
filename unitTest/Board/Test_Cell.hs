{-# LANGUAGE TemplateHaskell #-}
module Test_Cell
( test_cell
) where

import PseudoMacros
import TestKit
import Cell

test_cell :: IO ()      
test_cell = do  
    
      let aEmpty::Cell String
          aEmpty = Empty
          bEmpty::Cell String
          bEmpty = Empty
          
      tst_EQUAL True (aEmpty == bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL False (aEmpty /= bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL False (Fix "OK" == bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL False (aEmpty == Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL True (Fix "OK" /= bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL True (aEmpty /= Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))        
      tst_EQUAL False (Tmp "OK" == bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL False (aEmpty == Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL True (Tmp "OK" /= bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL True (aEmpty /= Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       

      tst_EQUAL True (Fix "OK" == Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL False (Fix "OK" /= Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
      tst_EQUAL False (Fix "OK" == Fix "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL True (Fix "OK" /= Fix "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   

      tst_EQUAL True (Fix "OK" == Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL True (Tmp "OK" == Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   
      tst_EQUAL False (Fix "OK" /= Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL False (Tmp "OK" /= Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))        
      tst_EQUAL False (Fix "OK" == Tmp "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL False (Tmp "UPS" == Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       
      tst_EQUAL True (Fix "OK" /= Tmp "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL True (Tmp "UPS" /= Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   

      tst_EQUAL True (Tmp "OK" == Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL False (Tmp "OK" /= Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
      tst_EQUAL False (Tmp "OK" == Tmp "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL True (Tmp "OK" /= Tmp "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      
      test_fix
      test_notFix
 
test_fix :: IO ()      
test_fix = do
     tst_EQUAL False (fix (Tmp "1"))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
     tst_EQUAL False (fix (Tmp (5::Int)))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
     tst_EQUAL False (fix Empty)
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
     tst_EQUAL True (fix (Fix "Hallo"))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
     
test_notFix :: IO ()      
test_notFix = do
     tst_EQUAL True (notFix (Tmp "1"))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
     tst_EQUAL True (notFix (Tmp (5::Int)))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
     tst_EQUAL True (notFix Empty)
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
     tst_EQUAL False (notFix (Fix "Hallo"))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      