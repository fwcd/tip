{-# LANGUAGE OverloadedStrings #-}
module Tip.Utils.General
    ( fromRight
    ) where

import qualified Data.Text as T

fromRight :: Either T.Text b -> b
fromRight (Right x) = x
fromRight (Left e) = error $ T.unpack $ "Expected Right, but got: " <> e
