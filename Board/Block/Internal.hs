module Block.Internal
( snippets
, snippetRow
, glueToBlocks
, blockRow
, frstBlock
) where

import Cell

     
snippets :: Int -> [[Cell String]] -> [[[Cell String]]]
snippets _ [] = []
snippets i (row:f) = 
      let snippetRow' = snippetRow i row
      in [snippetRow'] ++ snippets i f
            
snippetRow :: Int -> [Cell String]-> [[Cell String]]
snippetRow _ [] = []
snippetRow i xs = 
      let (ys,zs) = splitAt i xs   
      in [ys] ++ snippetRow i zs
-----
-----       
glueToBlocks :: Int -> [[[Cell String]]] -> [[[Cell String]]]
glueToBlocks _ [] = []
glueToBlocks i (x:xs) =
      let (line,_) = blockRow i x
      in [line] ++ (glueToBlocks i xs)

blockRow :: Int -> [[Cell String]] -> ([[Cell String]], [[Cell String]])
blockRow _ [] = ([],[])
blockRow i xs = 
      let (n,m) = frstBlock i xs
          (o,p) = blockRow i m
      in (([n] ++ o),p)

frstBlock :: Int -> [[Cell String]] -> ([Cell String], [[Cell String]])
frstBlock _ [] = ([],[])
frstBlock 1 (x:xs) = (x,xs)
frstBlock i (x:xs) =  
      let (n,m ) = frstBlock (i-1) xs
      in ((x ++ n),m)
         


