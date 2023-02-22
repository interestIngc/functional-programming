module HW0.T4
  (
    repeat'
  , map'
  , fib
  , fac
  ) where

import Data.Function (fix)
import Numeric.Natural

repeat' :: a -> [a]             -- behaves like Data.List.repeat
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b]  -- behaves like Data.List.map
map' f = fix $ \rec l ->
  case l of
    []       -> []
    (x : xs) -> (f x) : (rec xs)

fib :: Natural -> Natural       -- computes the n-th Fibonacci number
fib = snd . (fix $ \rec i ->
  case i of
    0 -> (0, 0)
    1 -> (0, 1)
    _ -> let (a, b) = rec (i - 1)
      in (b, a + b))

fac :: Natural -> Natural       -- computes the factorial
fac = fix $ \rec n ->
  case n of
    0 -> 1
    _ -> n * rec (n - 1)

