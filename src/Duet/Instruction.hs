{-# LANGUAGE NoImplicitPrelude #-}
module Duet.Instruction
    ( Instruction(..)
    )where

import Duet.Prelude
import Duet.Reference  ( Reference )
import Duet.RegisterId ( RegisterId )
import Duet.Value      ( Value )

data Instruction
  = Add RegisterId Reference
  | Jgz Reference Reference
  | Mod RegisterId Reference
  | Mul RegisterId Reference
  | Rcv Reference
  | Set Reference Value
  | Snd Reference
  deriving (Eq, Ord, Read, Show)
