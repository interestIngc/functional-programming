module HW1.T4
  (
    tfoldr
  ) where

import HW1.T3

-- | foldr implementation for the Tree data type.
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ z Leaf                          = z
tfoldr f res (Branch _ Leaf value Leaf)  = f value res
tfoldr f res (Branch _ Leaf value right) =
  f value (tfoldr f res right)
tfoldr f res (Branch _ left value Leaf)  =
  tfoldr f (f value res) left
tfoldr f res (Branch _ left value right) =
  tfoldr f (f value (tfoldr f res right)) left
