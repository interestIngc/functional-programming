module Main where

import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)
import System.Console.Haskeline
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop where
  loop :: InputT IO ()
  loop = do
    input <- getInputLine "% "
    case input of
      Nothing -> return ()
      Just "quit" -> return ()
      Just str -> 
        case (parse str) of
          (Left parseError) -> outputStrLn (errorBundlePretty parseError)
          (Right expr) -> 
            do
              eitherVal <- eval expr
              case eitherVal of
                (Left hiError) -> undefined
                (Right hiValue) -> do
                  outputStrLn $ show (prettyValue hiValue)
                  loop
