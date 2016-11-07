module Main where

import System.Environment

import qualified Parse.HtmlParser as HTML
import qualified Naming.VariableAssignment as Assignment
import qualified CodeGeneration.CodeGenerator as CG
import qualified Parse.Repeat as R

import qualified CodeGeneration.Javascript.JavascriptES6 as ES6

import Except.TypeDef

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

main :: IO ()
main = runEitherT run >>= \r -> case r of
  Right outputFilename -> putStrLn ("Output written at " ++ outputFilename)
  Left ex -> putStrLn $ exceptionMessage ex

run :: EitherT Exception IO String
run = do
  args <- lift getArgs
  input <- getArgValue "-i" args
  output <- getArgValue "-o" args
  html <- lift $ readFile input
  code <- processHtml html
  lift $ writeFile output code
  return output

getArgValue :: String -> [String] -> EitherT Exception IO String
getArgValue key (arg:(val:args))
  | key == arg = return val
  | otherwise = getArgValue key (val:args)

getArgValue key [_] = left ArgumentException

getArgValue key [] = left ArgumentException


processHtml :: String -> EitherT Exception IO String
processHtml html = hoistEither (CG.generateCode ES6.javascript . Assignment.assignNames <$> (R.withRepeat =<< HTML.parseHtml html))


exceptionMessage :: Exception -> String
exceptionMessage ex@(ParseException _) = show ex
exceptionMessage ex@ArgumentException = show ex ++ "\n" ++ usage


usage :: String
usage = "Usage: hendoman -i <input_file> -o <output_file>"
