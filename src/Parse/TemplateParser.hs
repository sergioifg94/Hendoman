module Parse.TemplateParser (parseText) where

  import Text.ParserCombinators.Parsec

  import Except.TypeDef
  import qualified Tree.TemplateExpression as TE

  parseText :: String -> Either Exception TE.Text
  parseText text = case parse parseTemplate "Text" text of
    Right result -> Right result
    Left _ -> Left $ ParseException "Failed parsing template expression"

  parseTemplate :: Parser TE.Text
  parseTemplate = do
    result <- many1 (try parseLiteral <|> parseExpression)
    eof
    return result

  parseExpression :: Parser TE.TemplateExpression
  parseExpression = do
    string "{{"
    expression <- parseString
    string "}}"
    return $ TE.Template expression


  parseLiteral :: Parser TE.TemplateExpression
  parseLiteral = TE.Literal <$> parseString

  parseString :: Parser String
  parseString = many1 (noneOf "{}")
