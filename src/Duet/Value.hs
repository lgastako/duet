{-# LANGUAGE NoImplicitPrelude #-}
module Duet.Value
    ( Value( Value )
    ) where

import Duet.Prelude

newtype Value = Value Int
  deriving (Eq, Ord, Read, Show)
