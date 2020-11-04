{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
            | Lambda a VarName (Expr a)       -- \x -> y
            | Let a VarName (Expr a) (Expr a) -- let v = x in y
            | Typed a (Expr a)                -- x :: t
    deriving (Show, Eq)

-- Extracts the type from the expression.
exprType :: Expr a -> a
exprType e = case e of
    LitStr t _   -> t
    LitInt t _   -> t
    Var t _      -> t
    Apply t  _ _ -> t
    Lambda t _ _ -> t
    Let t _ _ _  -> t
    Typed t _    -> t

-- Inserts explicit type annotation nodes at the
-- top-most level and at every let binding.
typeAnnotated :: Expr a -> Expr a
typeAnnotated e = Typed (exprType e) $ typeAnnotated' e
    where typeAnnotated' e = case e of
            Typed t x     -> typeAnnotated' x
            Apply t f x   -> Apply t (typeAnnotated' f) (typeAnnotated' x)
            Lambda t x e' -> Lambda t x $ typeAnnotated' e'
            Let t x e' b  -> Let t x (typeAnnotated e') (typeAnnotated b)
            _             -> e

instance (Show a, PrettyPrec a) => PrettyPrec (Expr a) where
    prettyPrec p = prettyPrec' p . typeAnnotated
        where prettyPrec' :: Int -> Expr a -> Doc ann
              prettyPrec' p e = case e of
                -- TODO: Figure out the correct precedences here
                Var _ v       -> parensIf (p > 3) $ pretty v
                LitStr _ s    -> parensIf (p > 3) $ "\"" <> pretty s <> "\""
                LitInt _ i    -> parensIf (p > 3) $ pretty i
                Apply _ f x   -> parensIf (p > 1) $ prettyPrec' 1 f <+> prettyPrec' 2 x
                Lambda _ x e' -> parensIf (p > 0) $ ("\\" <> pretty x <+> "->" <+> prettyPrec' 0 e')
                Let _ x e' b  -> parensIf (p > 0) $ vcat ["let" <+> pretty x <+> "=" <+> prettyPrec' 0 e', "in" <+> prettyPrec' 1 b]
                Typed t x     -> parensIf (p > 0) $ prettyPrec' 1 x <+> "::" <+> prettyPrec 0 t
