module HW0.T6
  (
    a
  , b
  , c
  , a_whnf
  , b_whnf
  , c_whnf
  ) where

import Data.Char (isSpace)
import HW0.T1

a :: (Either String b, Either String c)
a = distrib (Left ("AB" ++ "CD" ++ "EF"))     -- distrib from HW0.T1

b :: [Bool]
b = map isSpace "Hello, World"

c :: String
c = if 1 > 0 || error "X" then "Y" else "Z"

a_whnf :: (Either String b, Either String c)
a_whnf = (Left ("ABCDEF"), Left ("ABCDEF"))

b_whnf :: [Bool]
b_whnf = False : (map isSpace "ello, World")

c_whnf :: String
c_whnf = 'Y' : []
