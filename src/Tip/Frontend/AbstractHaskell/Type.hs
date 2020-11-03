{-# LANGUAGE OverloadedStrings #-}
module Tip.Frontend.AbstractHaskell.Type 
    ( Type (..)
    , Scheme (..)
    , generalize
    , free
    ) where

import Tip.Frontend.AbstractHaskell.VarName
import Tip.Utils.Pretty

-- A type that possibly contains free variables.
data Type = TypeStr
          | TypeInt
          | TypeVar VarName
          | TypeFun Type Type
    deriving (Show, Eq)

-- A type with universal quantifiers.
data Scheme = Scheme [VarName] Type
    deriving (Show, Eq)

-- Fetches a list of free type variables in the given type.
free :: Type -> [VarName]
free (TypeVar v) = [v]
free (TypeFun x y) = free x ++ free y
free _ = []

-- Binds all variables of a type with a universal quantifier.
generalize :: Type -> Scheme
generalize t = Scheme (free t) t

instance PrettyPrec Type where
    prettyPrec p t = case t of
        TypeStr     -> "String"
        TypeInt     -> "Int"
        TypeVar v   -> prettyPrec 0 v
        TypeFun x y -> parensIf (p > 0) $ prettyPrec 1 x <+> "->" <+> prettyPrec 0 y

instance PrettyPrec Scheme where
    prettyPrec _ (Scheme vs t) = "forall " <> (hsep $ prettyPrec 0 <$> vs) <> ". " <> prettyPrec 0 t
