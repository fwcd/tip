module Tip.Frontend.AST.Expr
    ( Expr (..)
    , value
    ) where

import Tip.Frontend.AST.VarName

-- An expression AST node.
data Show a => Expr a = LitStr a String                 -- "abc"
                      | LitInt a Int                    -- 123
                      | Var a String                    -- abc
                      | Apply a (Expr a) (Expr a)       -- x y
                      | Lambda a VarName (Expr a)      -- \x -> y
                      | Let a VarName (Expr a) (Expr a) -- let v = x in y
    deriving (Show, Eq)

-- Extracts the value from the expression.
value :: Show a => Expr a -> a
value e = case e of
    LitStr x _   -> x
    LitInt x _   -> x
    Var x _      -> x
    Apply x _ _  -> x
    Lambda x _ _ -> x
    Let x _ _ _  -> x
