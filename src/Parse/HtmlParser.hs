module Parse.HtmlParser (parseHtml) where

import Except.TypeDef

import Tree.HtmlTree
import qualified Parse.TemplateParser as TP
import qualified Tree.TemplateExpression as TE

import Text.HTML.TagSoup
import Text.ParserCombinators.Parsec

import qualified Data.Map as M


parseHtml :: String -> Either Exception [HtmlNode]
parseHtml input = case parse (many1 parseNode) "HTML" (parseTags input) of
  Left _ -> Left $ ParseException "Parsing error"
  Right nodes -> Right nodes

-- | Parses an HTML node
parseNode = try parseElement <|> parseText

-- | Parses an HTML element
parseElement = do
  (tagName, attributes) <- openTag
  children <- many parseNode
  closeTag tagName
  return $ HtmlElement Nothing tagName children attributes

-- | Parses a text node
parseText = HtmlText Nothing <$> textNode

-- | Parses the text from a text node
textNode = tokenPrim show update isText where
  isText :: Tag String -> Maybe TE.Text
  isText (TagText text) = case TP.parseText text of
    Right res -> Just res
    Left _ -> Nothing
  isText _ = Nothing

-- | Parses an element opening tag
openTag = tokenPrim show update isOpen where
  isOpen :: Tag String -> Maybe (String, M.Map String String)
  isOpen (TagOpen tagName attributes) = Just (tagName, M.fromList attributes)
  isOpen _ = Nothing

-- Parses the closing tag. The tag must match the one received
closeTag tag = tokenPrim show update (isClose tag) where
  isClose :: String -> Tag String -> Maybe ()
  isClose t (TagClose tc) | t == tc = Just () | otherwise = Nothing
  isClose _ _ = Nothing

update :: SourcePos -> Tag String -> [Tag String] -> SourcePos
update pos _ (tag:_) = incSourceColumn pos 1
update pos _ [] = pos
