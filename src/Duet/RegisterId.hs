{-# LANGUAGE NoImplicitPrelude #-}
module Duet.RegisterId
    ( RegisterId
    , a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
    , fromChar
    ) where

import Data.Char    ( isLower )
import Duet.Prelude

newtype RegisterId = RegisterId Char
  deriving (Eq, Ord, Read, Show)

fromChar :: Char -> Maybe RegisterId
fromChar ch
  | isLower ch = Just $ RegisterId ch
  | otherwise = Nothing

a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z :: RegisterId
Just a = fromChar 'a'
Just b = fromChar 'b'
Just c = fromChar 'c'
Just d = fromChar 'd'
Just e = fromChar 'e'
Just f = fromChar 'f'
Just g = fromChar 'g'
Just h = fromChar 'h'
Just i = fromChar 'i'
Just j = fromChar 'j'
Just k = fromChar 'k'
Just l = fromChar 'l'
Just m = fromChar 'm'
Just n = fromChar 'n'
Just o = fromChar 'o'
Just p = fromChar 'p'
Just q = fromChar 'q'
Just r = fromChar 'r'
Just s = fromChar 's'
Just t = fromChar 't'
Just u = fromChar 'u'
Just v = fromChar 'v'
Just w = fromChar 'w'
Just x = fromChar 'x'
Just y = fromChar 'y'
Just z = fromChar 'z'
