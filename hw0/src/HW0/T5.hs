module HW0.T5
  (
    Nat
  , nz
  , ns
  , nplus
  , nmult
  , nFromNatural
  , nToNum
  ) where


import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ x = x

ns :: Nat a -> Nat a
ns nat f x = nat f (f x)

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus nf mf f x = nf f (mf f x)
nmult nf mf f x = nf (mf f) x

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns $ nFromNatural (n - 1)

nToNum :: Num a => Nat a -> a
nToNum nat = nat (+ 1) 0
