{-# LANGUAGE NoImplicitPrelude #-}
module Duet
    ( someFunc
    ) where

import Duet.Prelude

main :: IO ()
main = interact interpret

interpret :: ()
interpret = undefined
