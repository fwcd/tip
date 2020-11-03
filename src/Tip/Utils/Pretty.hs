{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Tip.Utils.Pretty
    ( module Prettyprinter
    , PrettyPrec (..)
    , parensIf
    ) where

import qualified Data.Text as T
import Prettyprinter

class PrettyPrec a where
    prettyPrec :: Int -> a -> Doc ann

instance PrettyPrec T.Text where
    prettyPrec = const pretty

instance PrettyPrec Int where
    prettyPrec = const pretty

instance PrettyPrec () where
    prettyPrec = const pretty

parensIf :: Bool -> Doc ann -> Doc ann
parensIf p | p         = parens
           | otherwise = id
