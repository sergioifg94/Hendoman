module Parse.Repeat where

import Tree.HtmlTree

import qualified Data.Map as M
import Data.Maybe
import Text.ParserCombinators.Parsec

-- | Attribute that indicates that the node is repeated
repeatAttribute :: String
repeatAttribute = "data-repeat"

-- | Generates the given tree with repeat nodes
withRepeat :: [HtmlNode] -> Maybe [HtmlNode]
withRepeat = mapM visitNode

-- | Visits a node. If the repeat attribute is found, creates a repeated element node and inserts
-- | it above the node
visitNode :: HtmlNode -> Maybe HtmlNode
visitNode (HtmlElement varName tag childNodes attributes) = case M.lookup repeatAttribute attributes of
  Just repeatExpr -> do
    (var, repeater) <- getRepeat repeatExpr    -- Gets the repeating expressions
    childNodes' <- withRepeat childNodes       -- Visits the child nodes
    return $ HtmlRepeatedElement repeater var (HtmlElement varName tag childNodes' (cleanAttributes attributes))

  -- Visits the child nodes and creates the node with them
  Nothing -> withRepeat childNodes >>= \childNodes' -> return $ HtmlElement varName tag childNodes' attributes

visitNode node@(HtmlText _ _) = Just node

visitNode node@(HtmlComment _) = Just node

-- | Cleans the attributes map of the node, removing meta attributes
cleanAttributes :: M.Map String String -> M.Map String String
cleanAttributes = M.delete repeatAttribute

-------------------------------------
-- Parsing of the repeat attribute --
-------------------------------------

-- (Variable, Repeater)
type Repeat = (String, String)

-- Gets the repeating expression from the attribute value
getRepeat :: String -> Maybe Repeat
getRepeat input = case parse parseRepeatExpr "Repeat expression" input of
  Right repeat -> Just repeat
  Left _ -> Nothing

-- Parses the repeating expression
parseRepeatExpr :: Parser Repeat
parseRepeatExpr = do
  var <- parseVar
  parseIn
  repeater <- parseVar
  return (var, repeater)

parseVar :: Parser String
parseVar = many1 $ noneOf " "

parseIn :: Parser ()
parseIn = do
  optional spaces
  string "in"
  optional spaces
