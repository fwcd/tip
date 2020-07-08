module Tip.Frontend.AST.Expr
    ( Expr (..)
    ) where

import Tip.Frontend.AST.VarName

-- An expression AST node.
data Show a => Expr a = LitStr a String                 -- "abc"
                      | LitInt a Int                    -- 123
                      | Var a String                    -- abc
                      | Apply a (Expr a) (Expr a)       -- x y
                      | Lambda a (Expr a) (Expr a)      -- \x -> y
                      | Let a VarName (Expr a) (Expr a) -- let v = x in y
    deriving (Show, Eq)
