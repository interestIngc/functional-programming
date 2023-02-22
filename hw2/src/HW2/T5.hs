module HW2.T5
  (
    ExceptState (..)
  , EvaluationError (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import qualified Control.Monad
import HW2.T1 (Annotated (..), Except (..), mapAnnotated, mapExcept)
import HW2.T4 (Expr (..), Prim (..))

-- | State monad representation.
-- | Stores a function which takes current state 
-- and either returns `Error`, 
-- or `Annotated` value wrapped into `Success` with the result and new state.
data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

-- | Maps except states according to the given function.
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f state =
  ES (\s ->
    let
      exceptAnnotated = runES state s
    in mapExcept (mapAnnotated f) exceptAnnotated)

-- | Wraps a value into `ExceptState`.
wrapExceptState :: a -> ExceptState e s a
wrapExceptState a =
  ES (\s ->
    Success (a :# s))

-- | Join implementation for `ExceptState`.
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES runExceptState) =
  ES (\s ->
    case runExceptState s of
      (Error e)                            -> Error e
      (Success ((ES newRunState) :# newS)) -> newRunState newS)

-- | Modifies state according to the given function.
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f =
  ES (\s ->
    Success (() :# f s))

-- | Wraps an error into 'ExceptState'.
throwExceptState :: e -> ExceptState e s a
throwExceptState e =
  ES (\_ ->
    Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  ma >>= f = joinExceptState (mapExceptState f ma)

-- | Evaluation error representation.
data EvaluationError = DivideByZero

safeDivision :: Double -> Double -> Except EvaluationError Double
safeDivision _ 0 = Error DivideByZero
safeDivision x y = Success (x / y)

applyEvalBinary
  :: (Double -> Double -> Prim Double)
  -> (Double -> Double -> Except EvaluationError Double)
  -> Expr
  -> Expr
  -> ExceptState EvaluationError [Prim Double] Double
applyEvalBinary modifyStateFunction f left right = do
  leftValue <- eval left
  rightValue <- eval right
  modifyExceptState
    (\trace -> modifyStateFunction leftValue rightValue : trace)
  case f leftValue rightValue of
    (Error e)       -> throwExceptState e
    (Success value) -> wrapExceptState value

applyEvalUnary
  :: (Double -> Prim Double)
  -> (Double -> Double)
  -> Expr
  -> ExceptState EvaluationError [Prim Double] Double
applyEvalUnary modifyStateFunction f expr = do
  value <- eval expr
  modifyExceptState (\trace -> modifyStateFunction value : trace)
  wrapExceptState (f value)

-- | Evaluates the given expression.
-- Returns `ExceptState` holding either an `EvaluationError`, 
-- or the result of evaluation and evaluation trace.
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val value) = wrapExceptState value
eval (Op (Add left right))
  = applyEvalBinary Add (\x y -> Success (x + y)) left right
eval (Op (Sub left right))
  = applyEvalBinary Sub (\x y -> Success (x - y)) left right
eval (Op (Mul left right))
  = applyEvalBinary Mul (\x y -> Success (x * y)) left right
eval (Op (Div left right))
  = applyEvalBinary Div safeDivision left right
eval (Op (Abs expr)) = applyEvalUnary Abs abs expr
eval (Op (Sgn expr)) = applyEvalUnary Sgn signum expr
