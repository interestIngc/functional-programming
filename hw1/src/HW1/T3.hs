module HW1.T3
  (
    Tree (..)
  , tdepth
  , tFromList
  , tinsert
  , tmember
  , tsize
  ) where

-- | Binary tree representation.
data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a) deriving Show

-- | Returns size of the tree.
tsize :: Tree a -> Int
tsize Leaf                     = 0
tsize (Branch (size, _) _ _ _) = size

-- | Returns depth of the tree.
tdepth :: Tree a -> Int
tdepth = getHeight

-- | Checks if the element is in the tree.
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ l value r)
  | x == value = True
  | x < value  = tmember x l
  | otherwise  = tmember x r


getSize :: Tree a -> Int
getSize Leaf                     = 0
getSize (Branch (size, _) _ _ _) = size

getHeight :: Tree a -> Int
getHeight Leaf                       = 0
getHeight (Branch (_, height) _ _ _) = height

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left val right =
  Branch (getSize left + getSize right + 1,
              max (getHeight left) (getHeight right) + 1)
  left
  val
  right

diff :: Tree a -> Int
diff Leaf             = 0
diff (Branch _ l _ r) = getHeight l - getHeight r

rotateLeft :: Tree a -> Tree a
rotateLeft Leaf = Leaf
rotateLeft (Branch pr left value right) =
  case right of
    Leaf -> Branch pr left value right
    (Branch _ rightLeft valueRight rightRight) ->
      mkBranch (mkBranch left value rightLeft) valueRight rightRight

rotateRight :: Tree a -> Tree a
rotateRight Leaf = Leaf
rotateRight (Branch pr left value right) =
  case left of
    Leaf -> Branch pr left value right
    Branch _ leftLeft valueLeft leftRight ->
      mkBranch leftLeft valueLeft (mkBranch leftRight value right)

bigRotateLeft :: Tree a -> Tree a
bigRotateLeft Leaf                        = Leaf
bigRotateLeft (Branch _ left value right) =
  rotateLeft (mkBranch left value (rotateRight right))

bigRotateRight :: Tree a -> Tree a
bigRotateRight Leaf                        = Leaf
bigRotateRight (Branch _ left value right) =
  rotateRight (mkBranch (rotateLeft left) value right)

rotate :: Tree a -> Tree a
rotate Leaf = Leaf
rotate (Branch pr left value right) =
  let
    tree = Branch pr left value right
    balance = diff tree
    balanceLeft = diff left
    balanceRight = diff right
  in
    if balance == -2 && balanceRight == 1
    then bigRotateLeft tree
    else
      if balance == -2
      then rotateLeft tree
      else
        if balance == 2 && balanceLeft == -1
        then bigRotateRight tree
        else
          if balance == 2
          then rotateRight tree
          else tree

-- | Inserts an element into the tree.
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = Branch (1, 1) Leaf x Leaf
tinsert x (Branch pr l value r)
      | x == value = Branch pr l value r
      | x < value  = rotate $ mkBranch (tinsert x l) value r
      | otherwise  = rotate $ mkBranch l value (tinsert x r)

-- | Builds tree from the list.
tFromList :: Ord a => [a] -> Tree a
tFromList []     = Leaf
tFromList (x:xs) = tinsert x (tFromList xs)
