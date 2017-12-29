module Tool_List
( at
) where

--import Data.Char
--import Data.List
--import Data.Fixed
--import Field
--import Control.Exception
--import Block.Internal

at :: [a] -> Int -> a
at [] _ = error "maximum of empty list"
at (x:_) 0 = x
at (_:xs) i = xs `at` (i-1)  