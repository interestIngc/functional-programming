module HW2.T1
  (
    Option (..)
  , Pair (..)
  , Quad (..)
  , Annotated (..)
  , Except (..)
  , Prioritised (..)
  , Stream (..)
  , List (..)
  , Fun (..)
  , Tree (..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  ) where

import Prelude ()

-- | Representation of an optional value.
data Option a = None | Some a

-- | Maps `Option` values according to the given function.
mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None     = None
mapOption f (Some x) = Some (f x)

-- | Representation of pair.
data Pair a = P a a

-- | Maps `Pair` values according to the given function.
mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P x y) = P (f x) (f y)

-- | Representation of quad.
data Quad a = Q a a a a

-- | Maps `Quad` values according to the given function.
mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q first second third forth)
  = Q (f first) (f second) (f third) (f forth)

-- | Represents annotated values.
data Annotated e a = a :# e
infix 0 :#

-- | Maps `Annotated` values according to the given function.
mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

-- | Represents values with two possibilities:
-- | either an `Error`
-- | or `Success` with the result of calculation.
data Except e a = Error e | Success a

-- | Maps `Except` values according to the given function.
mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success (f a)

-- | Representation of values with priority.
data Prioritised a = Low a | Medium a | High a

-- | Maps `Prioritised` values according to the given function.
mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a)    = Low (f a)
mapPrioritised f (Medium a) = Medium (f a)
mapPrioritised f (High a)   = High (f a)

-- | Infinite stream representation.
data Stream a = a :> Stream a
infixr 5 :>

-- | Maps `Stream` values according to the given function.
mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (element :> stream) = f element :> mapStream f stream

-- | List representation.
data List a = Nil | a :. List a
infixr 5 :.

-- | Maps `List` values according to the given function.
mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil               = Nil
mapList f (element :. list) = f element :. mapList f list

-- | Function representation.
data Fun i a = F (i -> a)

-- | Maps `Fun` values according to the given function.
mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F g) = F (\x -> f (g x))

-- | Tree representation.
data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- | Maps `Tree` values according to the given function.
mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree f (Branch left value right)
  = Branch (mapTree f left) (f value) (mapTree f right)
