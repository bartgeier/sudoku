module Tool_List
( at
, ListZipper
, goForward
, goBack
) where

at :: [a] -> Int -> a
at [] _ = error "maximum of empty list"
at (x:_) 0 = x
at (_:xs) i = xs `at` (i-1)  

type ListZipper a = ([a],[a]) 

goForward :: ListZipper a -> ListZipper a  
goForward (ns,[]) = (ns, [])
goForward (ns, ms) = (ns ++ [head ms], (tail ms))  
  
goBack :: ListZipper a -> ListZipper a  
goBack ([], ns) = ([], ns)
goBack (ns, ms) = ((init ns), [last ns] ++ ms)
