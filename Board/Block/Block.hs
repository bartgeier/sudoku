module Block
( blocks
 ,blockIdx
) where

--import Data.Char
import Data.List
--import Data.Fixed
import Field
import Cell
import Control.Exception
import Block.Internal


          
blocks :: [[Cell String]] -> [[[Cell String]]]
blocks xs = assert (isField xs) $
      let grad' =  grad xs          
          snippets' = transpose (snippets grad' xs)
      in transpose $ glueToBlocks grad' snippets'      

blockIdx :: Int -> Int -> Int      
blockIdx grad' cellIdx = assert (cellIdx+1 <= grad'^(2::Int)) $
      ceiling ((fromIntegral cellIdx+1::Double) / (fromIntegral grad'::Double)) - 1

