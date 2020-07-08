module Tip.Frontend.AST.Type 
    ( Type (..)
    , Scheme (..)
    ) where

import Tip.Frontend.AST.VarName

data Type = TypeStr
          | TypeInt
          | TypeVar VarName
          | TypeApply Type Type
    deriving (Show, Eq)

data Scheme = Scheme [VarName] Type
    deriving (Show, Eq)
