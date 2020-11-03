module Tip.Frontend.AbstractHaskell.Unify
    ( unify
    ) where

import Tip.Frontend.AbstractHaskell.Subst
import Tip.Frontend.AbstractHaskell.Type
import Tip.Frontend.Utils.Pretty

-- Finds the most general unifier of two types.
unify :: Type -> Type -> Subst
unify (TypeFun x y) (TypeFun x' y') = let s1 = unify x x'
                                          s2 = unify (applySubst s1 y) (applySubst s1 y')
                                      in composeSubst s2 s1
unify (TypeVar v) t = varBindWithCheck v t
unify t (TypeVar v) = varBindWithCheck v t
unify TypeStr TypeStr = emptySubst
unify TypeInt TypeInt = emptySubst
unify t1 t2 = error $ "Non-unifiable types: " <> pretty t1 <> " and " <> pretty t2