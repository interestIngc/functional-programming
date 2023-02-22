module HW3.Base 
  (
    HiFun (..)
  , HiValue (..)
  , HiExpr (..)
  , HiError (..)
  , toString
  ) where

-- function names (e.g. div, sort, length, ...)
data HiFun = 
  HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub 
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  deriving (Eq, Show)
  
-- values (numbers, booleans, strings, ...)
data HiValue =
   HiValueNumber Rational
   | HiValueBool Bool
   | HiValueFunction HiFun
   deriving (Eq, Show)
   
-- expressions (literals, function calls, ...)
data HiExpr = 
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving (Eq, Show)
      
toString :: HiExpr -> String
toString = show

-- evaluation errors (invalid arguments, ...)
data HiError = 
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Eq, Show)


