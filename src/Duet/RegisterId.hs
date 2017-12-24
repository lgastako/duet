{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Duet.RegisterId
    ( RegisterId
    , fromChar
    , parse
    , a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
    ) where

import Data.Char    ( isLower )
import Duet.Prelude

newtype RegisterId = RegisterId Char
  deriving (Eq, Ord, Read, Show)

fromChar :: Char -> Maybe RegisterId
fromChar ch
  | isLower ch = Just $ RegisterId ch
  | otherwise  = Nothing

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

parse :: Text -> Maybe RegisterId
parse "a" = Just a
parse "b" = Just b
parse "c" = Just c
parse "d" = Just d
parse "e" = Just e
parse "f" = Just f
parse "g" = Just g
parse "h" = Just h
parse "i" = Just i
parse "j" = Just j
parse "k" = Just k
parse "l" = Just l
parse "m" = Just m
parse "n" = Just n
parse "o" = Just o
parse "p" = Just p
parse "q" = Just q
parse "r" = Just r
parse "s" = Just s
parse "t" = Just t
parse "u" = Just u
parse "v" = Just v
parse "w" = Just w
parse "x" = Just x
parse "y" = Just y
parse "z" = Just z
parse  _  = Nothing
