module NBoard
( Square(..)
, canditates
, emptyBoard
,numbering
,puzzel
,create
) where

import Data.List

data Square = Square { row :: Int  
                     , column :: Int  
                     , block :: Int   
                     , owner :: String  
                     , candidates :: [String]  
                     } deriving (Show)   

type Board = [Square]
type Puzzel = [Square]

create :: String -> Board
create str = 
      let owners = own (puzzel str) . numbering 0 . emptyBoard
          sheet = numbering 0 . emptyBoard
      in  fixIt (owners str) (sheet str)


emptyBoard :: String -> Board
emptyBoard str =
      let numOfRow = length [lineFeed | lineFeed <- str, lineFeed == '\n']
          numOfSquar = length [squa | squa <- str, squa == ',']
          numOfColumn =  quot numOfSquar numOfRow -- numOfSquar / numOfRow         
          s = Square {row = numOfRow
                     ,column = numOfColumn
                     ,block = 0
                     ,owner = ""
                     ,candidates = canditates str}
      in take numOfSquar (repeat s)
      

puzzel :: String -> [String] 
puzzel str =   
      lines [if x == ',' then '\n' else x  | x <- str, x /= '\n', x /= ' ' ]

own::[String]->Board->Board      
own _ [] = []  
own (p:ps) (Square{row = r, column = c, block = b, owner = o, candidates = cs }:board)
      | p == [] = own ps board
      | otherwise = let s = Square {row = r, column = c, block = b, owner = p, candidates = []}
                    in [s]++ own ps board
  
  
--fixIt :: [Square] -> Board -> Board
--fixIt sq b = map (\m -> setFix m b ) sq
fixIt :: [Square] -> Board -> Board
fixIt s b = map (\m -> cell s m)b

cell :: [Square] -> Square -> Square
cell [] b = b
cell (x:xs) b = 
      let n = setFix x b
      in cell xs n

setFix  :: Square -> Square -> Square
setFix s b = 
      if ((row s) == (row b) && (column s) /= (column b)
      || (row s) /= (row b) && (column s) == (column b))
            then b{candidates = (delete (owner s) (candidates b))}
            else if (row s) == (row b) && (column s) == (column b)
                  then b{candidates = (delete (owner s) (candidates b)), owner = (owner s) }
                  else b
                  


{-
setFix :: Square -> Board -> Board
setFix s b =  
      let b1 = map (\m -> 
            if (row s) == (row m) 
            then m{candidates = (delete (owner s) (candidates m))}
            else m) b
      in map (\m -> 
            if (column s) == (column m) 
            then m{candidates = (delete (owner s) (candidates m))}
            else m) b1
-}    
        
      
canditates :: String -> [String]
canditates str =
      let xs = nub (lines [if x == ',' then '\n' else x  | x <- str, x /= '\n', x /= ' ' ])
      in [c | c <- xs, c /= ""]
      
numbering :: Int -> Board -> Board  
numbering _ [] = []  
numbering idx (Square{row = r, column = c,owner = o, candidates = cs }:board) = 
      let r' = idx `quot` c
          c' = idx `mod` c
          b' = (r' `quot` (intSqrt r)) * (intSqrt c) + (c' `quot` (intSqrt c))      
          s = Square {row = r', column = c', block = b', owner = o, candidates = cs}
      in [s] ++ (numbering (idx+1) board )

intSqrt :: Int -> Int
intSqrt x = floor . sqrt $ (fromIntegral x :: Float)            