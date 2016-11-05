module CodeGeneration.CodeGenerator (generateCode) where

  import Control.Monad.Reader
  import Control.Monad.Writer
  import Tree.HtmlTree
  import CodeGeneration.JavascriptCode

  import qualified Data.Map as Map

  type CodeGeneration = WriterT String (Reader JavascriptCode) ()

  -- | Generate the JS code to create the nodes
  generateCode :: JavascriptCode -> [HtmlNode] -> String
  generateCode js nodes = let (_, code) = runReader (runWriterT $ mapM_ (writeNode Nothing) nodes) js
   in code


  writeNode :: Maybe HtmlNode -> HtmlNode -> CodeGeneration
  -- | Write a comment node. Writes the comment as a JS comment
  writeNode _ (HtmlComment text) = writeLine $ "/*" ++ text ++ " */"

  -- | Write a text node. Creates the text node and appends it to it's parent
  writeNode parent HtmlText { textVarName = Just varName, text = text'} = do
    js <- lift ask
    writeLine $ "var " ++ varName ++ " = document.createTextNode(" ++ stringTemplate js text' ++ ");"
    appendToParent parent varName

  -- | Write an element node
  writeNode parent node@HtmlElement { elementVarName = Just varName, tag = tag',
    children = children', attributes = attributes' } = do
      js <- lift ask
      writeLine $ "var " ++ varName ++ " = document.createElement(" ++ tag' ++ ");"
      writeAttributes varName attributes'
      forM_ children' (writeNode (Just node))      
      appendToParent parent varName

  -- | Write a repeated node
  writeNode parent HtmlRepeatedElement { repeater = repeater', variable = variable',
    node = node' } = do
      js <- lift ask
      (_, body) <- lift (runWriterT $ writeNode parent node')
      writeLine $ repeater' ++ ".forEach(" ++ writeFunction js [variable'] body ++ ");"


  -- | Write the code to append a node to its parent
  appendToParent :: Maybe HtmlNode -> String -> CodeGeneration
  appendToParent (Just HtmlElement { elementVarName = Just varName }) child =
    writeLine $ varName ++ ".appendChild(" ++ child ++ ");"

  appendToParent Nothing _ = return ()

  -- | Write the code for the attribute s of the node
  writeAttributes :: String -> Map.Map String String -> CodeGeneration
  writeAttributes varName attr = sequence_ $ Map.foldrWithKey
    (\k v acc -> (writeLine (varName ++ ".setAttribute(" ++ k ++ ", " ++ v ++ ");"):acc)) [] attr


  -- | Append a line to the current code
  writeLine :: String -> CodeGeneration
  writeLine = tell . (++ "\n")
