module Tip.Frontend.Check.TypeCheck (typeCheck) where

import Tip.Frontend.AST.Expr
import Tip.Frontend.AST.Type
import Tip.Frontend.AST.VarName

-- Type-checks the given AST and returns the typed AST
typeCheck :: Expr a -> Expr Type
typeCheck = error "Not implemented yet"
