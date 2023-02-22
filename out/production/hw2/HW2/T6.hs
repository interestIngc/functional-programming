{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  (
    ParseError (..)
  , Parser (..)
  , runP
  , pChar
  , parseError
  , pEof
  , parseExpr
  ) where

import Control.Applicative (Alternative, empty, optional, some, (<|>))
import Control.Monad (MonadPlus, mfilter)
import Data.Char
import qualified Data.Scientific
import HW2.T1 (Annotated (..), Except (..), mapExcept)
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5 (ExceptState (..), modifyExceptState)
import Numeric.Natural

-- | Parse error representation.
data ParseError = ErrorAtPos Natural

-- | Parser representation.
newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Runs given parser on a given string.
runP :: Parser a -> String -> Except ParseError a
runP (P state) str =
  let
    exceptAnnotated = runES state (0, str)
  in mapExcept (\(a :# _) -> a) exceptAnnotated

-- | Returns parser which parses one symbol.
pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

-- What happens when the string is empty?
-- It returns ParseError at the current position.
-- How does the parser state change when a character is consumed?
-- The position is incremented
-- and one character from the remaining input is consumed.

-- | Returns parser which always throws error.
parseError :: Parser a
parseError = P $ ES (\(pos, _) -> Error (ErrorAtPos pos))

instance Alternative Parser where
  empty = parseError
  (P stateLeft) <|> (P stateRight) = P $ ES
    (\s ->
      case runES stateLeft s of
        (Error _) -> runES stateRight s
        success   -> success)

instance MonadPlus Parser

-- | Parser which checks if the remaining string is empty.
pEof :: Parser ()
pEof = P $ ES
  \(pos, str) ->
    case str of
      [] -> Success (() :# (pos, str))
      _  -> Error (ErrorAtPos pos)

-- | Returns parser which skips leading whitespaces.
skipWhitespace :: Parser ()
skipWhitespace = P $ modifyExceptState
  (\(pos, str) ->
    let
      newStr = skip str
    in (pos, newStr))
  where
    skip :: String -> String
    skip []       = []
    skip (x : xs) = if x == ' ' then skip xs else x : xs

-- | Expects given char at the current position. 
-- Throws error if the string is empty or starts with a different char.
expectChar :: Char -> Parser Char
expectChar char = mfilter (== char) pChar <|> parseError

-- | Parses digits from the start of the string.
parseDigits :: Parser String
parseDigits = some (mfilter isDigit pChar)

-- | Parses number from the start of the string.
parseNumber :: Parser Double
parseNumber = do
  skipWhitespace
  integerPart <- parseDigits
  floatingPart <- (expectChar '.' >> parseDigits) <|> pure ""
  pure
    (doubleFromScientific
      (integerFromInt $ toInt (integerPart ++ floatingPart))
        ((-1) * length floatingPart))
  where
    integerFromInt :: Int -> Integer
    integerFromInt x = fromIntegral x :: Integer

    doubleFromScientific :: Integer -> Int -> Double
    doubleFromScientific value base = Data.Scientific.toRealFloat
      (Data.Scientific.scientific value base)

    toInt :: String -> Int
    toInt = fst . foldr
      (\char (number, acc) -> ((ord char - ord '0') * acc + number, acc * 10))
      (0, 1)

-- | Returns parser which verifies that next char matches the given char.
verifyNextToken :: Char -> Parser Char
verifyNextToken char = do
  skipWhitespace
  expectChar char

-- | Parses mul or div operation.
parseMulOrDiv :: Parser (Expr -> Expr -> Prim Expr)
parseMulOrDiv = (verifyNextToken '*' >> pure Mul)
  <|> (verifyNextToken '/' >> pure Div)

-- | Parses add or sub operation.
parseAddOrSub :: Parser (Expr -> Expr -> Prim Expr)
parseAddOrSub = (verifyNextToken '+' >> pure Add)
  <|> (verifyNextToken '-' >> pure Sub)

-- | Parses factor, i.e. either a number, 
-- or expression wrapped into parentheses.
parseFactor :: Parser Expr
parseFactor = (Val <$> parseNumber)
  <|> (verifyNextToken '(' >> parseExpression <* verifyNextToken ')')

-- | Parses arithmetic expression.
parseExpression :: Parser Expr
parseExpression = combine parseItem parseAddOrSub parseItem

-- | Parses factor sequences joined with mul or div.
parseItem :: Parser Expr
parseItem = combine parseFactor parseMulOrDiv parseFactor

-- | Parses sequences of expressions joined with the given operations.
combine
  :: Parser Expr
  -> Parser (Expr -> Expr -> Prim Expr)
  -> Parser Expr
  -> Parser Expr
combine parser parserOfOperation parserOfItem = do
  left <- parser
  maybeOp <- optional parserOfOperation
  case maybeOp of
    Nothing -> pure left
    (Just op) -> do
      right <- parserOfItem
      combine (pure (Op $ op left right)) parserOfOperation parserOfItem

-- | Validates that the string does not contain trailing trash. 
-- Throws error if the string is invalid, 
-- returns parsed expression if it is valid.
validateExpr :: Parser Expr
validateExpr = do
  expr <- parseExpression
  skipWhitespace
  pEof
  pure expr

-- | Parses a given string. 
-- Returns `Except` with either `ParseError`, or `Expr`.
parseExpr :: String -> Except ParseError Expr
parseExpr = runP validateExpr
