module CommandLine (getOptions) where

import Options

import Except.TypeDef
import Control.Monad.Trans.Either

import Control.Monad.Reader

import CodeGeneration.JavascriptCode
import qualified CodeGeneration.Javascript.JavascriptES6 as ES6
import qualified CodeGeneration.Javascript.JavascriptES5 as ES5

-- | Get the options from the arguments
getOptions :: [String] -> Either Exception Options
getOptions = runReader $ runEitherT optionsFromArgs

optionsFromArgs :: EitherT Exception (Reader [String]) Options
optionsFromArgs = do
  args <- lift ask
  outputWriter <- getOutputWriter
  inputReader <- getInputReader
  js <- lift getJavascript
  return $ Options inputReader outputWriter js


getArgValue :: String -> [String] -> Either Exception String
getArgValue key (arg:(val:args))
  | key == arg = return val
  | otherwise = getArgValue key (val:args)

getArgValue key [_] = Left ArgumentException

getArgValue key [] = Left ArgumentException


getJavascript :: Reader [String] JavascriptCode
getJavascript = do
  args <- ask
  if "--es5" `elem` args then
    return ES5.javascript
  else
    return ES6.javascript

getOutputWriter :: EitherT Exception (Reader [String]) (String -> IO ())
getOutputWriter = do
  args <- lift ask
  if "-o" `elem` args then
    hoistEither (getArgValue "-o" args) >>= \outputPath ->
     return $ (>>) (putStrLn $ "Written at " ++ outputPath) . writeFile outputPath
  else
    return putStrLn

getInputReader :: EitherT Exception (Reader [String]) (IO String)
getInputReader = do
  args <- lift ask
  if "-i" `elem` args then
    hoistEither $ readFile <$> getArgValue "-i" args
  else
    return getContents
