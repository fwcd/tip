module Tip.Frontend.AST.Type 
    ( Type (..)
    , Scheme (..)
    , free
    ) where

import Data.List (intercalate)
import Tip.Frontend.AST.Pretty
import Tip.Frontend.AST.VarName

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

instance Pretty Type where
    pretty t = case t of
        TypeStr -> "String"
        TypeInt -> "Int"
        TypeVar v -> v
        TypeFun x y -> "(" <> pretty x <> " -> " <> pretty y <> ")"

instance Pretty Scheme where
    pretty (Scheme vs t) = "forall " <> intercalate " " vs <> ". " <> pretty t
