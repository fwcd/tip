{-# LANGUAGE OverloadedStrings #-}
module Tip.Utils.Pretty
    ( Pretty (..)
    ) where

import qualified Data.Text as T

class Pretty a where
    pretty :: a -> T.Text

instance Pretty () where
    pretty = const "?"
