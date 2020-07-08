module Tip.Frontend.AST.Expr
    ( Expr
    ) where

type VarName = String

-- An expression AST node.
data Expr = LitStr String         -- "abc"
          | LitInt Int            -- 123
          | Apply Expr Expr       -- x y
          | Lambda Expr Expr      -- \x -> y
          | Let VarName Expr Expr -- let v = x in y
    deriving (Show, Eq)