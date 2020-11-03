{-# LANGUAGE OverloadedStrings #-}
module Tip.Frontend.AbstractHaskell.Expr
    ( Expr (..)
    , value
    ) where

import qualified Data.Text as T
import Prettyprinter (Pretty (..))
import Tip.Frontend.AbstractHaskell.VarName

-- An expression AST node.
data Expr a = LitStr a T.Text                 -- "abc"
            | LitInt a Int                    -- 123
            | Var a T.Text                    -- abc
            | Apply a (Expr a) (Expr a)       -- x y
            | Lambda a VarName (Expr a)      -- \x -> y
            | Let a VarName (Expr a) (Expr a) -- let v = x in y
    deriving (Show, Eq)

-- Extracts the value from the expression.
value :: Expr a -> a
value e = case e of
    LitStr x _   -> x
    LitInt x _   -> x
    Var x _      -> x
    Apply x _ _  -> x
    Lambda x _ _ -> x
    Let x _ _ _  -> x

instance Pretty a => Pretty (Expr a) where
    pretty e = case e of
        LitStr t s -> "\"" <> pretty s <> "\" :: " <> pretty t
        LitInt t i -> pretty i <> " :: " <> pretty t
        Var t v -> pretty v <> " :: " <> pretty t
        Apply t f x -> "(" <> pretty f <> " " <> pretty x <> ") :: " <> pretty t
        Lambda t x e' -> "(\\" <> pretty x <> " -> " <> pretty e' <> ") :: " <> pretty t
        Let t x e' b -> "(let " <> pretty x <> " = " <> pretty e' <> " in " <> pretty b <> ") :: " <> pretty t
