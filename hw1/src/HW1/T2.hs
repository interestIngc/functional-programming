module HW1.T2
  (
    N (..)
  , ncmp
  , ndiv
  , nEven
  , nFromNatural
  , nmod
  , nmult
  , nOdd
  , nplus
  , nsub
  , nToNum
  ) where

import Numeric.Natural

-- | Natural numbers representation.
data N = Z | S N deriving Show

-- | Addition.
nplus :: N -> N -> N
nplus x Z      = x
nplus x (S y') = S $ nplus x y'

-- | Multiplication.
nmult :: N -> N -> N
nmult _ Z      = Z
nmult x (S y') = nplus (nmult x y') x

-- | Subtraction, returns Nothing if result is negative.
nsub :: N -> N -> Maybe N
nsub Z Z           = Just Z
nsub Z (S _)       = Nothing
nsub x Z           = Just x
nsub (S x') (S y') = nsub x' y'

-- | Comparison.
ncmp :: N -> N -> Ordering
ncmp x y = case nsub x y of
  Nothing -> LT
  Just Z  -> EQ
  _       -> GT

-- | Returns N matching the given Natural value.
nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S $ nFromNatural (x - 1)

-- | Returns Num matching the given N value.
nToNum :: Num a => N -> a
nToNum Z      = 0
nToNum (S x') = nToNum x' + 1

-- | Check the parity of the given N value.
nEven, nOdd :: N -> Bool
nEven Z      = True
nEven (S x') = not (nEven x')

nOdd Z      = False
nOdd (S x') = not (nOdd x')

-- | Integer division.
ndiv :: N -> N -> N
ndiv x y = let sub = nsub x y
           in case sub of
             Nothing    -> Z
             Just value -> S $ ndiv value y

-- | Modulo operation.
nmod :: N -> N -> N
nmod x y = let sub = nsub x $ nmult (ndiv x y) y
           in case sub of
             Nothing    -> Z
             Just value -> value
