{-# LANGUAGE NoImplicitPrelude #-}
module Duet.Reference
    ( Reference(..)
    )where

import Duet.Prelude
import Duet.RegisterId ( RegisterId )
import Duet.Value      ( Value )

data Reference
  = RegisterRef RegisterId
  | ValueRef    Value
  deriving (Eq, Ord, Read, Show)
