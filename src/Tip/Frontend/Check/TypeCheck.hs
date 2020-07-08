module Tip.Frontend.Check.TypeCheck (typeCheck) where

import Control.Monad.State (State (..), runState, put, get)
import Tip.Frontend.AST.Expr
import Tip.Frontend.AST.Subst
import Tip.Frontend.AST.Type
import Tip.Frontend.AST.VarName

-- The type check monad holding a counter for fresh type variables.
type TM a = State Int a

-- Type-checks the given AST and returns the typed AST
typeCheck :: Show a => Expr a -> Expr Type
typeCheck = snd . runTM . infer emptyContext

-- Runs the type check monad.
runTM :: TM a -> a
runTM s = fst $ runState s 0

-- Produces a new type variable
freshTypeVar :: TM Type
freshTypeVar = do
    n <- get
    put (n + 1)
    return $ TypeVar $ "_" <> show n

-- Instantiates a type scheme using fresh variables.
instantiate :: Scheme -> TM Type
instantiate (Scheme vs t) = do
    s <- subst <$> zip vs <$> mapM (const freshTypeVar) vs
    return $ applySubst s t

-- Performs type inference and thereby generates a typed AST.
infer :: Show a => Context -> Expr a -> TM (Subst, Expr Type)
infer ctx (Apply _ f x) = do
    tyRes <- freshTypeVar
    (s1, f') <- infer ctx f
    (s2, x') <- infer (applySubstCtx s1 ctx) x
    let tyF = value f'
        tyX = value x'
    return (composeSubst s2 s1, Apply tyRes f' x')
infer ctx (Let _ x e b) = do
    (s1, e') <- infer ctx e
    (s2, b') <- infer (applySubstCtx s1 ctx) b
    let tyE = value e'
        tyB = value b'
    return (composeSubst s2 s1, Let tyB x e' b')
infer ctx (Var _ v) = case contextLookup v ctx of
    Just s -> do
        t <- instantiate s
        pure (emptySubst, Var t v)
    Nothing -> error $ "Unbound variable " <> v
infer _ (LitInt _ i) = pure (emptySubst, LitInt TypeInt i)
infer _ (LitStr _ s) = pure (emptySubst, LitStr TypeStr s)
