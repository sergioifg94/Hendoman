module CodeGeneration.JavascriptCode where

  import qualified Tree.TemplateExpression as TE

  data JavascriptCode = JavascriptCode {
    -- | Write the JS code for an anonymous function
    writeFunction :: [String] -> String -> String,

    -- | Check if a word is reserved in JS
    isReserved :: String -> Bool,

    -- | Write a string from a template
    stringTemplate :: TE.Text -> String
  }
