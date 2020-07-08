module Tip.Frontend.Check.TypeCheck (typeCheck) where

import Control.Monad.State (State (..), runState, put, get)
import qualified Data.Map as M
import Tip.Frontend.AST.Expr
import Tip.Frontend.AST.Subst
import Tip.Frontend.AST.Type
import Tip.Frontend.AST.VarName

-- The type check monad holding a counter for fresh type variables.
type TM a = State Int a

-- Type-checks the given AST and returns the typed AST
typeCheck :: Expr a -> Expr Type
typeCheck = error "Not implemented yet"

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
    s <- Subst <$> M.fromList <$> zip vs <$> mapM (const freshTypeVar) vs
    return $ applySubst s t

-- Performs type inference to find.
infer :: Expr a -> TM Type
infer = undefined
