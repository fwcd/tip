module Tip.Frontend.AbstractHaskell.Pretty
    ( Pretty (..)
    ) where

class Pretty a where
    pretty :: a -> String

instance Pretty () where
    pretty = const "?"
