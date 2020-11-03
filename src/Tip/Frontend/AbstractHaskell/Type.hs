{-# LANGUAGE OverloadedStrings #-}
module Tip.Frontend.AbstractHaskell.Type 
    ( Type (..)
    , Scheme (..)
    , generalize
    , free
    ) where

import Prettyprinter (Pretty (..), hsep)
import Tip.Frontend.AbstractHaskell.VarName

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

instance Pretty Type where
    pretty t = case t of
        TypeStr -> "String"
        TypeInt -> "Int"
        TypeVar v -> pretty v
        TypeFun x y -> "(" <> pretty x <> " -> " <> pretty y <> ")"

instance Pretty Scheme where
    pretty (Scheme vs t) = "forall " <> (hsep $ pretty <$> vs) <> ". " <> pretty t
