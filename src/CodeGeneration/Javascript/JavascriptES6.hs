module CodeGeneration.Javascript.JavascriptES6 (javascript) where

import CodeGeneration.JavascriptCode
import Tree.TemplateExpression

import Data.List

javascript :: JavascriptCode
javascript = JavascriptCode writeFunctionES6 (`notElem` reservedKeywords) stringInterpolation


writeFunctionES6 :: [String] -> String -> String
writeFunctionES6 params body = "(" ++ writeParams params ++ ") => { " ++ body ++ "}"


writeParams :: [String] -> String
writeParams = intercalate ", "


stringInterpolation :: Text -> String
-- Just text, use simple string
stringInterpolation [Literal lit] = "\"" ++ lit ++ "\""

-- Just an expression, write without quotes
stringInterpolation [Template content] = content

-- Both text and expressions, use string interpolatin
stringInterpolation text = foldl write "`" text ++ "`" where
  write :: String -> TemplateExpression -> String
  write acc (Literal lit) = acc ++ lit
  write acc (Template content) = acc ++ "${" ++ content ++ "}"

-- Reserved keywords in JS ES6
reservedKeywords :: [String]
reservedKeywords = ["let", "const"]
