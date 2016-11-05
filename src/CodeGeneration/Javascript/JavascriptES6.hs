module CodeGeneration.Javascript.JavascriptES6 (javascript) where

import CodeGeneration.JavascriptCode
import Tree.TemplateExpression

javascript :: JavascriptCode
javascript = JavascriptCode writeFunctionES6 (const False) stringInterpolation


writeFunctionES6 :: [String] -> String -> String
writeFunctionES6 params body = "(" ++ writeParams params ++ ") => { " ++ body ++ "}"


writeParams :: [String] -> String
writeParams = foldl ((++) . (++ ", ")) ""


stringInterpolation :: Text -> String
stringInterpolation [Literal lit] = "\"" ++ lit ++ "\""

stringInterpolation [Template content] = content

stringInterpolation text = foldl write "`" text ++ "`" where
  write :: String -> TemplateExpression -> String
  write acc (Literal lit) = acc ++ lit
  write acc (Template content) = acc ++ "${" ++ content ++ "}"
