module HW2.T3
  (
    joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  , getFun
  ) where

import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..))
import HW2.T2 (concatLists)

-- | Join implementation for `Option` values.
joinOption    :: Option (Option a) -> Option a
joinOption None          = None
joinOption (Some option) = option

-- | Join implementation for `Except` values.
joinExcept    :: Except e (Except e a) -> Except e a
joinExcept (Error e)        = Error e
joinExcept (Success except) = except

-- | Join implementation for `Annotated` values.
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# ea) :# e) = a :# (e <> ea)

-- | Join implementation for `List` values.
joinList      :: List (List a) -> List a
joinList Nil                 = Nil
joinList (firstList :. rest) = concatLists firstList (joinList rest)

-- | Retrieves function from a `Fun` value.
getFun :: Fun i a -> i -> a
getFun (F f) = f

-- | Join implementation for `Fun` values.
joinFun       :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\x -> getFun (f x) x)
