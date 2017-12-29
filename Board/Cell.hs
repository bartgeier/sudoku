module Cell
( Cell(Fix, Tmp, Empty)
, fix
, notFix
) where

data Cell a = Fix a | Tmp a | Empty deriving (Show)
instance (Eq m) => Eq (Cell m) where
      Fix x == Fix y = x == y
      Fix x == Tmp y = x == y
      Tmp x == Fix y = x == y
      Tmp x == Tmp y = x == y
      Empty == Empty = True
      _ == _  = False

fix :: Cell a -> Bool
fix (Fix _) = True
fix (Tmp _) = False
fix Empty = False

notFix :: Cell a -> Bool
notFix x = not $ fix x

