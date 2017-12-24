{-# LANGUAGE NoImplicitPrelude #-}
module Duet.Main
    ( main
    ) where

import Duet.Interpreter ( interpret1 )
import Focus.Prelude

main :: IO ()
main = interact interpret1


-- main2 :: IO ()
-- main2 = do
--   s <- getContents
--   out <- interpret s
--   putLn out
