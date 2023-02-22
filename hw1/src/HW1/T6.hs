module HW1.T6
  (
    epart
  , mcat
  ) where

concatMaybe :: Monoid a => Maybe a -> a -> a
concatMaybe Nothing res  = res
concatMaybe (Just x) res = x <> res

-- | Folds the list of Maybe a into a, assuming a has instance of Monoid.
mcat :: Monoid a => [Maybe a] -> a
mcat = foldr concatMaybe mempty

concatEither :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
concatEither (Left x) (a, b)  = (x <> a, b)
concatEither (Right y) (a, b) = (a, y <> b)

-- | Folds the list of Either a b into (a, b)
-- assuming a and b have instance of Monoid.
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr concatEither (mempty, mempty)

