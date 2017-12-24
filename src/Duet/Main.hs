{-# LANGUAGE NoImplicitPrelude #-}
module Duet.Main
    ( main
    ) where

import Duet.Interpreter ( interpret )
import Focus.Prelude

-- main :: IO ()
-- main = interact interpret

main :: IO ()
main = do
  s <- getContents
  out <- interpret s
  putLn out
