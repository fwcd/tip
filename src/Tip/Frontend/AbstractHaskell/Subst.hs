{-# LANGUAGE OverloadedStrings #-}
module Tip.Frontend.AbstractHaskell.Subst
    ( Subst (..)
    , Context (..)
    , emptySubst, emptyContext, subst
    , varBindWithCheck, varBindCtx
    , applySubst, applySubstScheme, applySubstCtx
    , composeSubst
    , contextLookup
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Tip.Frontend.AbstractHaskell.Type
import Tip.Frontend.AbstractHaskell.VarName
import Tip.Utils.Pretty

-- A substitution on types.
data Subst = Subst (M.Map VarName Type)

-- Holds a context of variable types.
data Context = Context (M.Map VarName Scheme)

-- An empty substitution (containing no mappings).
emptySubst :: Subst
emptySubst = Subst M.empty

-- An empty context (containing no typings).
emptyContext :: Context
emptyContext = Context M.empty

-- Constructs a substitution from a list of mappings.
subst :: [(VarName, Type)] -> Subst
subst = Subst . M.fromList

-- Binds a variable name in a substitution,
-- performing an occurs check.
varBindWithCheck :: VarName -> Type -> Subst
varBindWithCheck v (TypeVar v') | v == v' = emptySubst
varBindWithCheck v t            | elem v $ free t = error $ T.unpack $ "Occurs check: " <> v <> " is contained in " <> pretty t <> " (thus cannot be bound)"
                       | otherwise = subst [(v, t)]

-- Binds a variable name in a context.
varBindCtx :: VarName -> Scheme -> Context -> Context
varBindCtx v s (Context m) = Context $ M.insert v s m

-- Applies the substitution to a type.
applySubst :: Subst -> Type -> Type
applySubst s@(Subst m) t = case t of
    TypeVar   v   -> fromMaybe t $ M.lookup v m
    TypeFun   x y -> TypeFun   (applySubst s x) (applySubst s y)
    _ -> t

-- Applies a substitution to a type scheme by only
-- substituting free (unbound) variables.
applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme (Subst m) (Scheme vs t) = Scheme vs $ applySubst (Subst m') t
    where m' = M.filterWithKey (\k _ -> not $ elem k vs) m

-- Applies the substitution to a context.
applySubstCtx :: Subst -> Context -> Context
applySubstCtx s (Context m) = Context $ M.map (applySubstScheme s) m

-- Yields a subtitution that has the same effect as
-- first applying the right and then the left one
composeSubst :: Subst -> Subst -> Subst
composeSubst s1@(Subst m1) (Subst m2) = Subst $ M.union (M.map (applySubst s1) m2) m1

-- Looks up a type in a context.
contextLookup :: VarName -> Context -> Maybe Scheme
contextLookup v (Context m) = M.lookup v m
