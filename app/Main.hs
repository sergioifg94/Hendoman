module Main where

import System.Environment

import qualified Parse.HtmlParser as HTML
import qualified Naming.VariableAssignment as Assignment
import qualified CodeGeneration.CodeGenerator as CG
import qualified Parse.Repeat as R

import CodeGeneration.JavascriptCode

import Except.TypeDef

import CommandLine

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

import Options

main :: IO ()
main = runEitherT run >>= \r -> case r of
  Right _ -> return ()
  Left ex -> putStrLn $ exceptionMessage ex

run :: EitherT Exception IO ()
run = do
  args <- lift getArgs
  options <- hoistEither $ getOptions args
  html <- lift $ inputReader options
  code <- processHtml html (javascript options)
  lift $ outputWriter options code


processHtml :: String -> JavascriptCode -> EitherT Exception IO String
processHtml html js = hoistEither (CG.generateCode js . Assignment.assignNames <$> (R.withRepeat =<< HTML.parseHtml html))


exceptionMessage :: Exception -> String
exceptionMessage ex@(ParseException _) = show ex
exceptionMessage ex@ArgumentException = show ex ++ "\n" ++ usage


usage :: String
usage = "Usage: hendoman -i <input_file> -o <output_file>"
