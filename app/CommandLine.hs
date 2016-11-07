module CommandLine (getOptions) where

import Options

import Except.TypeDef
import Control.Monad.Trans.Either

import Control.Monad.Reader

import CodeGeneration.JavascriptCode
import qualified CodeGeneration.Javascript.JavascriptES6 as ES6

-- | Get the options from the arguments
getOptions :: [String] -> Either Exception Options
getOptions = runReader $ runEitherT optionsFromArgs

optionsFromArgs :: EitherT Exception (Reader [String]) Options
optionsFromArgs = do
  args <- lift ask
  input <- hoistEither $ getArgValue "-i" args
  outputWriter <- getOutputWriter
  js <- lift getJavascript
  return $ Options input outputWriter js


getArgValue :: String -> [String] -> Either Exception String
getArgValue key (arg:(val:args))
  | key == arg = return val
  | otherwise = getArgValue key (val:args)

getArgValue key [_] = Left ArgumentException

getArgValue key [] = Left ArgumentException


getJavascript :: Reader [String] JavascriptCode
getJavascript = do
  args <- ask
  if "--es6" `elem` args then
    return ES6.javascript
  else
    return ES6.javascript

getOutputWriter :: EitherT Exception (Reader [String]) (String -> IO ())
getOutputWriter = do
  args <- ask
  if "-o" `elem` args then
    hoistEither (getArgValue "-o" args) >>= \output ->
     (return $ \o -> putStrLn ("Written at " ++ output) >> writeFile output o)
  else
    return putStrLn