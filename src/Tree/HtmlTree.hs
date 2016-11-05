module Tree.HtmlTree where

  import qualified Data.Map as Map
  import qualified Tree.TemplateExpression as TemplateExpression

  data HtmlNode =
    HtmlElement {
      elementVarName :: Maybe String,
      tag :: String,
      children :: [HtmlNode],
      attributes :: Map.Map String String
    }
    |
    HtmlText {
      textVarName :: Maybe String,
      text :: TemplateExpression.Text
    }
    |
    HtmlComment String
    |
    HtmlRepeatedElement {
      repeater :: String,
      variable :: String,
      node :: HtmlNode
    }
    deriving (Show)
