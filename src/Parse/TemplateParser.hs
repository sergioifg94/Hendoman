module Parse.TemplateParser (parseText) where

  import Text.ParserCombinators.Parsec

  import qualified Tree.TemplateExpression as TE

  parseText :: String -> Maybe TE.Text
  parseText text = case parse parseTemplate "Text" text of
    Right result -> Just result
    Left _ -> Nothing

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
