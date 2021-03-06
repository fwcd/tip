{-# LANGUAGE OverloadedStrings #-}
module Tip.Frontend.Check.TypeCheck (typeCheck) where

import Control.Monad.State (State, runState, put, get)
import qualified Data.Text as T
import Tip.Frontend.AbstractHaskell.Expr
import Tip.Frontend.AbstractHaskell.Subst
import Tip.Frontend.AbstractHaskell.Type
import Tip.Frontend.AbstractHaskell.Unify

-- The type check monad holding an (infinite) list of fresh type variables.
type TM a = State [T.Text] a

-- Type-checks the given AST and returns the typed AST
typeCheck :: Show a => Expr a -> Expr Type
typeCheck = snd . runTM . infer emptyContext

-- An infinite list of fresh type variables.
freshTypeVars :: [T.Text]
freshTypeVars = alphabet ++ (freshTypeVars >>= \x -> map (x <>) alphabet)
    where alphabet = map T.singleton "abcdefghijklmnopqrstuvwxyz"

-- Runs the type check monad.
runTM :: TM a -> a
runTM s = fst $ runState s freshTypeVars

-- Produces a new type variable
freshTypeVar :: TM Type
freshTypeVar = do
    vs <- get
    let (v:vs') = vs
    put vs'
    return $ TypeVar $ "_" <> v

-- Instantiates a type scheme using fresh variables.
instantiate :: Scheme -> TM Type
instantiate (Scheme vs t) = do
    s <- subst <$> zip vs <$> mapM (const freshTypeVar) vs
    return $ applySubst s t

-- Performs type inference and thereby generates a typed AST.
infer :: Show a => Context -> Expr a -> TM (Subst, Expr Type)
infer ctx (Apply _ f x) = do
    -- Creates a new type var for the result, infers function and argument
    -- type, then unifies the inferred function type with the expected one.
    tyRes <- freshTypeVar
    (s1, f') <- infer ctx f
    (s2, x') <- infer (applySubstCtx s1 ctx) x
    let tyF = exprType f'
        tyX = exprType x'
        s3 = unify tyF $ TypeFun tyX tyRes
    return (foldr1 composeSubst [s3, s2, s1], Apply (applySubst s3 tyRes) f' x')

infer ctx (Lambda _ x e) = do
    -- Creates a new type var from the binder, puts it into a context
    -- and infers the body expression from there.
    tyX <- freshTypeVar
    let innerCtx = varBindCtx x (Scheme [] tyX) ctx
    (s, e') <- infer innerCtx e
    let tyRes = exprType e'
    return (s, Lambda (TypeFun (applySubst s tyX) tyRes) x e')

infer ctx (Let _ x e b) = do
    -- First infer the type of the expression,
    -- then infer the body using the generalized/universally quantified type (with 'forall')
    (s1, e') <- infer ctx e
    let tyE = exprType e'
        innerCtx = varBindCtx x (generalize tyE) ctx
    (s2, b') <- infer (applySubstCtx s1 innerCtx) b
    let tyB = exprType b'
    return (composeSubst s2 s1, Let tyB x e' b')

infer ctx (Var _ v) = case contextLookup v ctx of
    Just s -> do
        t <- instantiate s
        pure (emptySubst, Var t v)
    Nothing -> error $ T.unpack $ "Unbound variable " <> v

infer _ (LitInt _ i) = pure (emptySubst, LitInt TypeInt i)
infer _ (LitStr _ s) = pure (emptySubst, LitStr TypeStr s)
