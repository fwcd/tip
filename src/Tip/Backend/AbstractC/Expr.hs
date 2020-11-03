{-# LANGUAGE OverloadedStrings #-}
module Tip.Backend.AbstractC.Expr
    ( Expr (..)
    ) where

import qualified Data.Text as T
import Data.List (intersperse)
import Prettyprinter (Pretty (..), hsep)

data Expr = LitStr T.Text       -- "test"
          | LitInt Int          -- 42
          | Var T.Text          -- x
          | Apply T.Text [Expr] -- f(x, y, z, ...)

instance Pretty Expr where
    pretty e = case e of
        LitStr s -> "\"" <> pretty s <> "\""
        LitInt i -> pretty i
        Var v -> pretty v
        Apply f xs -> pretty f <> "(" <> (hsep $ intersperse ", " $ pretty <$> xs) <> ")"

