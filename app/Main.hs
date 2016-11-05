module Main where

import System.Environment

import qualified Parse.HtmlParser as HTML
import qualified Naming.VariableAssignment as Assignment
import qualified CodeGeneration.CodeGenerator as CG

import qualified CodeGeneration.Javascript.JavascriptES6 as ES6

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

main :: IO ()
main = runMaybeT run >>= \r -> case r of
  Just _ -> print "Aye"
  Nothing -> print "Error"

run :: MaybeT IO ()
run = do
  args <- liftIO getArgs
  input <- getArgValue "-i" args
  output <- getArgValue "-o" args
  html <- liftIO $ readFile input
  code <- processHtml html
  liftIO $ writeFile output code

getArgValue :: String -> [String] -> MaybeT IO String
getArgValue key (arg:(val:args))
  | key == arg = return val
  | otherwise = getArgValue key (val:args)

getArgValue key [_] = MaybeT $ return Nothing

processHtml :: String -> MaybeT IO String
processHtml html = MaybeT $ return (CG.generateCode ES6.javascript . Assignment.assignNames <$> HTML.parseHtml html)
