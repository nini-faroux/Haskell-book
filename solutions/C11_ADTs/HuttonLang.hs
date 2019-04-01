module C11_ADTs.HuttonLang where

data Expr = 
    Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer 
eval (Lit n) = n 
eval (Add ex1 ex2) = (eval ex1) + (eval ex2)

-- $ printExpr (Add (Lit 1) (Lit 9001))
-- "1 + 9001"
printExpr :: Expr -> String 
printExpr (Lit n) = show n
printExpr (Add ex1 ex2) = (printExpr ex1) ++ " + " ++ (printExpr ex2)
