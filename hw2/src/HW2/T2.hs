module HW2.T2
  (
    distOption
  , distPair
  , distQuad
  , distAnnotated
  , distExcept
  , distPrioritised
  , distStream
  , distList
  , distFun
  , wrapOption
  , wrapPair
  , wrapQuad
  , wrapAnnotated
  , wrapExcept
  , wrapPrioritised
  , wrapStream
  , wrapList
  , wrapFun
  , concatLists
  ) where

import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..), Pair (..),
               Prioritised (..), Quad (..), Stream (..))
import Prelude (Monoid, Semigroup, mempty, (<>))

-- | Maps pair of `Option` values into an `Option` value storing a pair.
distOption      :: (Option a, Option b) -> Option (a, b)
distOption (None, _)        = None
distOption (_, None)        = None
distOption (Some x, Some y) = Some (x, y)

-- | Wraps value into `Option` value.
wrapOption      :: a -> Option a
wrapOption = Some

-- | Maps pair of `Pair` values into a `Pair` value storing a pair.
distPair        :: (Pair a, Pair b) -> Pair (a, b)
distPair (P ax ay, P bx by) = P (ax, bx) (ay, by)

-- | Wraps value into `Pair` value.
wrapPair        :: a -> Pair a
wrapPair x = P x x

-- | Maps pair of `Quad` values into a `Quad` value storing a pair.
distQuad        :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q aFirst aSecond aThird aForth, Q bFirst bSecond bThird bForth) =
  Q (aFirst, bFirst) (aSecond, bSecond) (aThird, bThird) (aForth, bForth)

-- | Wraps value into `Quad` value.
wrapQuad        :: a -> Quad a
wrapQuad x = Q x x x x

-- | Maps pair of `Annotated` values into an `Annotated` value storing a pair.
distAnnotated
  :: Semigroup e
  => (Annotated e a, Annotated e b)
  -> Annotated e (a, b)
distAnnotated (a :# ea, b :# eb) = (a, b) :# (ea <> eb)

-- | Wraps value into `Annotated` value.
wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

-- | Maps pair of `Except` values into an `Except` value storing a pair.
distExcept      :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e
distExcept (Success a, Success b) = Success (a, b)

-- | Wraps value into `Except` value.
wrapExcept      :: a -> Except e a
wrapExcept = Success

-- | Maps pair of `Prioritised` values 
-- into a `Prioritised` value storing a pair.
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, High b)     = High (a, b)

-- | Wraps value into `Prioritised` value.
wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

-- | Maps pair of `Stream` values into a `Stream` value storing a pair.
distStream      :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> streamA, b :> streamB)
  = (a, b) :> distStream (streamA, streamB)

-- | Wraps value into `Stream` value.
wrapStream      :: a -> Stream a
wrapStream x = x :> wrapStream x

-- | Concatenates two lists.
concatLists :: List p -> List p -> List p
concatLists Nil list2          = list2
concatLists (x :. rest1) list2 = x :. concatLists rest1 list2

-- | Maps pair of `List` values into a `List` value storing a pair.
distList        :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (x :. restA, listB)
  = concatLists (mapToPair x listB) (distList (restA, listB)) where
  mapToPair :: a -> List b -> List (a, b)
  mapToPair _ Nil = Nil
  mapToPair elementA (elementB :. restB)
    = (elementA, elementB) :. mapToPair elementA restB

-- | Wraps value into `List` value.
wrapList        :: a -> List a
wrapList x = x :. Nil

-- | Maps pair of `Fun` values into a `Fun` value storing a pair.
distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\x -> (f x, g x))

-- | Wraps value into `Fun` value.
wrapFun         :: a -> Fun i a
wrapFun x = F (\_ -> x)
