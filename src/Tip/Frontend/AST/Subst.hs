module Tip.Frontend.AST.Subst
    ( Subst (..)
    , applySubst
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Tip.Frontend.AST.Type
import Tip.Frontend.AST.VarName

-- A substitution on types.
data Subst = Subst (M.Map VarName Type)

-- Applies the substitution to a type.
applySubst :: Subst -> Type -> Type
applySubst s@(Subst m) t = case t of
    TypeVar   v   -> fromMaybe t $ M.lookup v m
    TypeApply x y -> TypeApply (applySubst s x) (applySubst s y)
    TypeFun   x y -> TypeFun   (applySubst s x) (applySubst s y)
    _ -> t
