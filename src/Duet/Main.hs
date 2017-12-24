{-# LANGUAGE NoImplicitPrelude #-}
module Duet.Main
    ( main
    ) where

import Duet.Interpreter ( interpret )
import Focus.Prelude

main :: IO ()
main = interact interpret
