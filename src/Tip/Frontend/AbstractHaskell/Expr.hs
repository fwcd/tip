{-# LANGUAGE OverloadedStrings #-}
module Tip.Frontend.AbstractHaskell.Expr
    ( Expr (..)
    , exprType
    ) where

import qualified Data.Text as T
import Tip.Frontend.AbstractHaskell.VarName
import Tip.Utils.Pretty

-- An expression AST node.
data Expr a = LitStr a T.Text                 -- "abc"
            | LitInt a Int                    -- 123
            | Var a T.Text                    -- abc
            | Apply a (Expr a) (Expr a)       -- x y
            | Lambda a VarName (Expr a)      -- \x -> y
            | Let a VarName (Expr a) (Expr a) -- let v = x in y
    deriving (Show, Eq)

-- Extracts the type from the expression.
exprType :: Expr a -> a
exprType e = case e of
    LitStr x _   -> x
    LitInt x _   -> x
    Var x _      -> x
    Apply x _ _  -> x
    Lambda x _ _ -> x
    Let x _ _ _  -> x

instance PrettyPrec a => PrettyPrec (Expr a) where
    prettyPrec p e = case e of
        LitStr t s    -> "\"" <> prettyPrec 0 s <> "\"" <+> "::" <+> prettyPrec 0 t
        LitInt t i    -> prettyPrec 0 i <+> "::" <+> prettyPrec 0 t
        Var t v       -> prettyPrec 0 v <+> "::" <+> prettyPrec 0 t
        Apply t f x   -> parensIf (p > 1) $ prettyPrec 2 f <+> prettyPrec 1 x <+> "::" <+> prettyPrec 0 t
        Lambda t x e' -> "\\" <> prettyPrec 0 x <+> "->" <+> prettyPrec 0 e' <+> "::" <+> prettyPrec 0 t
        Let t x e' b  -> vcat ["let" <+> prettyPrec 0 x <+> "=" <+> prettyPrec 0 e', "in" <+> prettyPrec 0 b <+> "::" <+> prettyPrec 0 t]
