module HW1.T7
  (
    DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

-- | Non empty list representation.
data ListPlus a = a :+ ListPlus a | Last a deriving Show
infixr 5 :+

instance Semigroup (ListPlus a) where
  (Last x) <> rhs    = x :+ rhs
  (x :+ rest) <> rhs = x :+ (rest <> rhs)

-- | 'Inclusive' data type represents values with three possibilities:
-- 'Inclusive a b' is either 'This a', or 'That b' or 'Both a b'.
data Inclusive a b = This a | That b | Both a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (This a) <> (This x)     = This (a <> x)
  (This a) <> (That b)     = Both a b
  (This a) <> (Both x b)   = Both (a <> x) b
  (That b) <> (This a)     = Both a b
  (That b) <> (That y)     = That (b <> y)
  (That b) <> (Both a y)   = Both a (b <> y)
  (Both a b) <> (This x)   = Both (a <> x) b
  (Both a b) <> (That y)   = Both a (b <> y)
  (Both a b) <> (Both x y) = Both (a <> x) (b <> y)

-- | Representation of String, such that
-- two DotString values are concatenated with a dot.
newtype DotString = DS String deriving Show

instance Semigroup DotString where
  (DS "") <> (DS str)    = DS str
  (DS str) <> (DS "")    = DS str
  (DS str1) <> (DS str2) = DS (str1 ++ ('.' : str2))

instance Monoid DotString where
  mempty = DS ""

-- | 'Fun a' represents functions a -> a.
newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (F f) <> (F g) = F (f . g)

instance Monoid (Fun a) where
  mempty = F id
