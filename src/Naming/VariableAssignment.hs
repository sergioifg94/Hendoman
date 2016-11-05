module Naming.VariableAssignment (assignNames) where

  import Tree.HtmlTree

  import Control.Monad.State
  import Data.Maybe
  import qualified Data.Map as M

  -- | Map with the existing names. The key is the name, and the value
  -- | the index for that name
  type ExistingNames = M.Map String Int

  data AssignmentState = AssignmentState {
    textNodesIndex :: Int,
    elementNodesIndex :: Int,
    existingNames :: ExistingNames
  }

  type Assignment a = State AssignmentState a

  -- | Traverses the tree and assigns variable names to the nodes, creating a
  -- | new tree
  assignNames :: [HtmlNode] -> [HtmlNode]
  assignNames nodes = let (nodes', _) = runState (forM nodes assignName) initialState
    in nodes'

  -- | Assign a variable name to a node
  assignName :: HtmlNode -> Assignment HtmlNode
  assignName node@(HtmlElement _ t c a) = do {
    st <- get;
    -- | If the node doesn't have an ID, assign it the default name and increase
    -- | the index
    if not (hasId node) then do
      modify addElementName
      c' <- assignChildren c
      return $ HtmlElement (Just ("node" ++ show (elementNodesIndex st))) t c' a
    else do
      id <- getId a
      c' <- assignChildren c
      return $ HtmlElement (Just id) t c' a
    } where
      assignChildren :: [HtmlNode] -> Assignment [HtmlNode]
      assignChildren = mapM assignName

  assignName (HtmlText _ text) = do
    st <- get
    let result = HtmlText (Just ("text" ++ show (textNodesIndex st))) text
    modify addTextName
    return result

  assignName (HtmlRepeatedElement repeater var node) =
    HtmlRepeatedElement repeater var <$> assignName node

  assignName comment@(HtmlComment _) = return comment


  initialState :: AssignmentState
  initialState = AssignmentState {
    textNodesIndex = 0,
    elementNodesIndex = 0,
    existingNames = M.empty
  }

  addTextName :: AssignmentState -> AssignmentState
  addTextName (AssignmentState t e n) = AssignmentState (t + 1) e n

  addElementName :: AssignmentState -> AssignmentState
  addElementName (AssignmentState t e n) = AssignmentState t (e + 1) n

  addExistingName :: String -> AssignmentState -> AssignmentState
  addExistingName name (AssignmentState t e n) = case M.lookup name n of
    Just i -> AssignmentState t e (M.insert name (i + 1) n)

  createExistingName :: String -> AssignmentState -> AssignmentState
  createExistingName name (AssignmentState t e n) =
    AssignmentState t e (M.insert name 1 n)

  hasId :: HtmlNode -> Bool
  hasId (HtmlElement _ _ _ attr) = isJust $ M.lookup "id" attr

  getId :: M.Map String String -> Assignment String
  getId attributes = do
    st <- get
    let id = fromJust $ M.lookup "id" attributes
    case M.lookup id (existingNames st) of
      Just i -> modify (addExistingName id) >> return (id ++ show (i + 1))
      Nothing -> modify (createExistingName id) >> return id
