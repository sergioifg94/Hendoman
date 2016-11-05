module Tree.TemplateExpression where

  type Text = [TemplateExpression]

  data TemplateExpression =
    Literal String |
    Template String
    
    deriving (Show, Eq)
