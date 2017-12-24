{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
module Duet.Value
    ( Value( Value )
    , parse
    ) where

import Duet.Prelude

newtype Value = Value Int
  deriving (Enum, Eq, Integral, Num, Ord, Read, Real, Show)

parse :: Text -> Maybe Value
parse = (Value <$>) . readMaybe
