{-# LANGUAGE OverloadedStrings #-}
module Tip.Backend.AbstractC.Expr
    ( Expr (..)
    ) where

import qualified Data.Text as T
import Tip.Utils.Pretty

data Expr = LitStr T.Text       -- "test"
          | LitInt Int          -- 42
          | Var T.Text          -- x
          | Apply T.Text [Expr] -- f(x, y, z, ...)

instance Pretty Expr where
    pretty e = case e of
        LitStr s -> "\"" <> s <> "\""
        LitInt i -> T.pack $ show i
        Var v -> v
        Apply f xs -> f <> "(" <> T.intercalate ", " (pretty <$> xs) <> ")"

