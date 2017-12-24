{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
module Duet.Value
    ( Value( Value )
    , parse
    ) where

import Duet.Prelude

newtype Value = Value Int
  deriving (Eq, Num, Ord, Read, Show)

parse :: Text -> Maybe Value
parse = (Value <$>) . readMaybe

-- TODO: eliminate me
-- TODO: wtf do I need this?
instance Monoid Value where
  mempty = Value 0
  mappend (Value a) (Value b) = Value . getSum $ mappend (Sum a) (Sum b)
