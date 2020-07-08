module Tip.Frontend.AST.Type 
    ( Type (..)
    , Scheme (..)
    ) where

import Tip.Frontend.AST.VarName

-- A type that possibly contains free variables.
data Type = TypeStr
          | TypeInt
          | TypeVar VarName
          | TypeFun Type Type
          | TypeApply Type Type
    deriving (Show, Eq)

-- A type with universal quantifiers.
data Scheme = Scheme [VarName] Type
    deriving (Show, Eq)
