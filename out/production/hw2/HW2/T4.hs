module HW2.T4
  (
    State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import qualified Control.Monad
import HW2.T1 (Annotated (..), mapAnnotated)

-- | State monad representation.
-- Stores a function which takes current state 
-- and returns `Annotated` value with the result and new state.
data State s a = S { runS :: s -> Annotated s a }

-- | Maps states according to the given function.
mapState :: (a -> b) -> State s a -> State s b
mapState f (S runState) =
  S (\s ->
      let
        annotatedA = runState s
      in mapAnnotated f annotatedA)

-- | Wraps a value into `State`.
wrapState :: a -> State s a
wrapState a = S (a :#)

-- | Join implementation for `State`.
joinState :: State s (State s a) -> State s a
joinState (S runState) =
  S (\s ->
      let
        (S runInnerState) :# se = runState s
      in runInnerState se)

-- | Modifies state according to the given function.
modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

-- | Arithmetic operation representation.
data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum

-- | Arithmetic expression representation.
data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)

  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
   x / y = Op (Div x y)
   fromRational x = Val (fromRational x)

applyEvalBinary
  :: (Double -> Double -> Prim Double)
  -> (Double -> Double -> Double)
  -> Expr
  -> Expr
  -> State [Prim Double] Double
applyEvalBinary modifyStateFunction f left right = do
  leftValue <- eval left
  rightValue <- eval right
  modifyState (\trace -> modifyStateFunction leftValue rightValue : trace)
  wrapState (f leftValue rightValue)

applyEvalSingle
  :: (Double -> Prim Double)
  -> (Double -> Double)
  -> Expr
  -> State [Prim Double] Double
applyEvalSingle modifyStateFunction f expr = do
   value <- eval expr
   modifyState (\trace -> modifyStateFunction value : trace)
   wrapState (f value)

-- | Evaluates the given expression.
-- Returns `State` holding the result of evaluation and evaluation trace.
eval :: Expr -> State [Prim Double] Double
eval (Val value)           = wrapState value
eval (Op (Add left right)) = applyEvalBinary Add (+) left right
eval (Op (Sub left right)) = applyEvalBinary Sub (-) left right
eval (Op (Mul left right)) = applyEvalBinary Mul (*) left right
eval (Op (Div left right)) = applyEvalBinary Div (/) left right
eval (Op (Abs expr))       = applyEvalSingle Abs abs expr
eval (Op (Sgn expr))       = applyEvalSingle Sgn signum expr


