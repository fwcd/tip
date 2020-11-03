module Tip.Frontend.AbstractC.Expr
    ( Expr (..)
    ) where

import Data.List (intercalate)
import Tip.Frontend.Utils.Pretty

data Expr = LitStr String       -- "test"
          | LitInt Int          -- 42
          | Var String          -- x
          | Apply String [Expr] -- f(x, y, z, ...)

instance Pretty Expr where
    pretty e = case e of
        LitStr s -> "\"" <> s <> "\""
        LitInt i -> show i
        Var v -> v
        Apply f xs -> f <> "(" <> intercalate ", " (pretty <$> xs) <> ")"

