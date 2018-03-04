{-# LANGUAGE TemplateHaskell #-}
module Test_Cell
( test_cell
) where

import PseudoMacros
import TestKit
import Cell

test_cell :: UnitTestState -> IO ()     
test_cell this = do  
    
      let aEmpty::Cell String
          aEmpty = Empty
          bEmpty::Cell String
          bEmpty = Empty
          
      tst_EQUAL this True (aEmpty == bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this False (aEmpty /= bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      
      tst_EQUAL this False (Fix "OK" == bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this False (aEmpty == Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL this True (Fix "OK" /= bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this True (aEmpty /= Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))        
      tst_EQUAL this False (Tmp "OK" == bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this False (aEmpty == Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
      tst_EQUAL this True (Tmp "OK" /= bEmpty)
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this True (aEmpty /= Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       

      tst_EQUAL this True (Fix "OK" == Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this False (Fix "OK" /= Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
      tst_EQUAL this False (Fix "OK" == Fix "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this True (Fix "OK" /= Fix "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   

      tst_EQUAL this True (Fix "OK" == Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL this True (Tmp "OK" == Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   
      tst_EQUAL this False (Fix "OK" /= Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL this False (Tmp "OK" /= Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))        
      tst_EQUAL this False (Fix "OK" == Tmp "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL this False (Tmp "UPS" == Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))       
      tst_EQUAL this True (Fix "OK" /= Tmp "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      tst_EQUAL this True (Tmp "UPS" /= Fix "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))   

      tst_EQUAL this True (Tmp "OK" == Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this False (Tmp "OK" /= Tmp "OK")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))    
      tst_EQUAL this False (Tmp "OK" == Tmp "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))
      tst_EQUAL this True (Tmp "OK" /= Tmp "UPS")
      putStrLn("cell, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
      
      test_fix this
      test_notFix this
 
test_fix :: UnitTestState -> IO ()     
test_fix this = do
     tst_EQUAL this False (fix (Tmp "1"))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
     tst_EQUAL this False (fix (Tmp (5::Int)))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
     tst_EQUAL this False (fix Empty)
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
     tst_EQUAL this True (fix (Fix "Hallo"))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
     
test_notFix :: UnitTestState -> IO ()   
test_notFix this = do
     tst_EQUAL this True (notFix (Tmp "1"))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))  
     tst_EQUAL this True (notFix (Tmp (5::Int)))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
     tst_EQUAL this True (notFix Empty)
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int)) 
     tst_EQUAL this False (notFix (Fix "Hallo"))
     putStrLn("fix, " ++ $__FILE__ ++ ", line " ++ show (($__LINE__)::Int))      