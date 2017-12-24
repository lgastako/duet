{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Duet.Reference
    ( Reference(..)
    , parse
    ) where

import           Duet.Prelude
import           Duet.RegisterId               ( RegisterId )
import qualified Duet.RegisterId as RegisterId
import           Duet.Value                    ( Value )
import qualified Duet.Value      as Value

data Reference
  = RegisterRef RegisterId
  | ValueRef    Value
  deriving (Eq, Ord, Read, Show)

parse :: Text -> Maybe Reference
parse s = (RegisterRef <$> RegisterId.parse s <|> ValueRef <$> Value.parse s)
