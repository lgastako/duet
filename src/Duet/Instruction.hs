{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Duet.Instruction
    ( Instruction(..)
    , parse
    ) where

import qualified Data.Text       as Text
import           Duet.Prelude
import           Duet.Reference                ( Reference )
import qualified Duet.Reference  as Reference
import           Duet.RegisterId               ( RegisterId )
import qualified Duet.RegisterId as RegisterId

data Instruction
  = Add RegisterId Reference
  | Jgz Reference Reference
  | Mod RegisterId Reference
  | Mul RegisterId Reference
  | Rcv Reference
  | Set RegisterId Reference
  deriving (Eq, Ord, Read, Show)

parse :: Text -> Maybe Instruction
parse s = case instr of
  "add" -> Just $ Add regId ref1
  "jgz" -> Just $ Jgz ref0  ref1
  "mod" -> Just $ Mod regId ref1
  "mul" -> Just $ Mul regId ref1
  "rcv" -> Just $ Rcv ref0
  "set" -> Just $ Set regId ref1
  _     -> Nothing
  where
    Just instr = headMay pieces
    Just args  = tailMay pieces

    Just arg0  = headMay args
    Just arg1  = lastMay args

    pieces     = Text.words s

    Just regId = RegisterId.parse arg0
    Just ref0  = Reference.parse  arg0
    Just ref1  = Reference.parse  arg1
