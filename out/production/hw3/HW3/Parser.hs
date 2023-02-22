{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser 
  (
    parse
  , showParse
  ) where

import qualified Text.Megaparsec as TM
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import Data.Void (Void)
--import Data.Text (Text)
import HW3.Base
import Text.Megaparsec (try, choice, (<|>), many, optional, eof, runParser)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (scientific, symbol)
import Text.Megaparsec.Debug (dbg)
import Data.Functor (void)
import Control.Monad.Combinators.Expr (makeExprParser, Operator (..))
import Control.Monad (MonadPlus)

type Parser a = TM.Parsec Void String a

skipWhitespace :: Parser ()
skipWhitespace = void (many (char ' '))

expectChar :: Char -> Parser Char
expectChar ch = do
  dbg "expectChar skip whitespace" skipWhitespace
  dbg "expectChar char" (char ch)

parseRational :: Parser Rational
parseRational = do
  skipWhitespace
  sign <- optional (try $ char '-')
  number <- scientific
  case sign of
    Nothing -> return (toRational number)
    (Just _) -> return (-1 * (toRational number))

parseNumber :: Parser HiExpr
parseNumber = (HiExprValue . HiValueNumber) <$> parseRational

parseBool :: Parser HiExpr
parseBool = (HiExprValue . HiValueBool) <$> 
  (skipWhitespace *> choice
    [ True <$ string "true"
    , False <$ string "false"
    ])
  
parseArguments :: Parser [HiExpr]
parseArguments = do
  first <- parseExpr
  maybeNext <- optional (try $ expectChar ',')
  case maybeNext of
    Nothing -> return ([first])
    (Just _) -> do
      args <- parseArguments
      return (first : args)
  
parseFunctionName :: Parser HiExpr
parseFunctionName = (HiExprValue . HiValueFunction) <$> 
  (skipWhitespace *> choice
    [ HiFunDiv <$ string "div"
    , HiFunMul <$ string "mul"
    , HiFunAdd <$ string "add"
    , HiFunSub <$ string "sub"
    , HiFunNot <$ string "not"
    , HiFunAnd <$ string "and"
    , HiFunOr <$ string "or"
    , HiFunLessThan <$ string "less-than"
    , HiFunGreaterThan <$ string "greater-than"
    , HiFunEquals <$ string "equals"
    , HiFunNotLessThan <$ string "not-less-than"
    , HiFunNotGreaterThan <$ string "not-greater-than"
    , HiFunNotEquals <$ string "not-equals"
    , HiFunIf <$ string "if"
    ])
  
parseFunction :: Parser HiExpr
parseFunction = do
  function <- parseFunctionName
  dbg "parseFunction expect (" $ void (expectChar '(')
  args <- dbg "parseFunction parse args" parseArguments
  dbg "parseFunction expect )" $ void (expectChar ')')
  return (HiExprApply function args)

parseExpr :: Parser HiExpr
parseExpr = try parseFunction <|> try parseBool <|> parseNumber

expr :: Parser HiExpr
expr = makeExprParser term table

term :: Parser HiExpr
term = undefined

table :: MonadPlus m => [[Operator m HiExpr]]
table = [ [] ]

--binary :: Text -> ()  

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (parseExpr <* skipWhitespace <* eof) ""

showParse :: String -> String
showParse str = 
  case (parse str) of
    (Left parseError) -> show (errorBundlePretty parseError)
    (Right expr) -> show expr


