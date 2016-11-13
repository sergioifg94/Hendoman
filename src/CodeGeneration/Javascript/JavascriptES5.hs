module CodeGeneration.Javascript.JavascriptES5 (javascript) where

import CodeGeneration.JavascriptCode
import Tree.TemplateExpression

import Data.List

javascript :: JavascriptCode
javascript = JavascriptCode writeFunctionES5 (const True) stringConcatenation


writeFunctionES5 :: [String] -> String -> String
writeFunctionES5 params body = "function (" ++ writeParams params ++ ") { " ++ body ++ "}"


writeParams :: [String] -> String
writeParams = intercalate ", "


stringConcatenation :: Text -> String
-- Just text, use simple string
stringConcatenation [Literal lit] = "\"" ++ lit ++ "\""

-- Just an expression, write without quotes
stringConcatenation [Template content] = content

-- Both text and expressions, use string interpolatin
stringConcatenation text = init $ init $ init (foldl write "" text) where
  write :: String -> TemplateExpression -> String
  write acc (Literal lit) = acc ++ "\"" ++ lit ++ "\" + "
  write acc (Template content) = acc ++ content ++ " + "
