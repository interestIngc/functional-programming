module HW1.T5
  (
    joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..))

-- | Splits the list on the separator.
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = [] :| []
splitOn pattern (x:xs) = let
                         (sub:|rest) = splitOn pattern xs
                         in if x == pattern
                            then [] :| (sub:rest)
                            else (x:sub) :| rest

-- | Joins the list with the separator.
joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (x:|[])             = x
joinWith pattern (x :| (y:xs)) = x ++ (pattern : (joinWith pattern (y:|xs)))
