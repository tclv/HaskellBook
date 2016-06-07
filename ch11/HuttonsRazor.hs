module HuttonsRazor where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit num) = num
eval (Add a b) = (eval a) + (eval b)

printExpr :: Expr -> String
printExpr (Lit num) = show num
printExpr (Add a b) = (printExpr a) ++ " + " ++ (printExpr b)

