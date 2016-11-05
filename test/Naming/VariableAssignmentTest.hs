module Naming.VariableAssignmentTest (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Naming.VariableAssignment
import Tree.HtmlTree

import qualified Data.Map as M

nodeTest :: HtmlNode
nodeTest = HtmlElement Nothing "div" [HtmlText Nothing []] (M.insert "id" "node" M.empty)

nodeNoID :: HtmlNode
nodeNoID = HtmlElement Nothing "div" [] M.empty

testSimpleNaming = TestCase $ do
  let named = head $ assignNames [nodeTest]
  let (HtmlText (Just textName) _) = head $ children named
  Just "node" @=? elementVarName named
  "text0" @=? textName


testRepeatedNames = TestCase $ do
  let named = assignNames [nodeTest, nodeTest]
  let (Just firstName) = elementVarName $ head named
  let (Just scndName) = elementVarName $ named !! 1
  assertBool "Elements with same id should still get different variable names" (firstName /= scndName)
  assertEqual "First node should get his ID as variable name" "node" firstName
  assertEqual "Second node should get a number after the ID" "node2" scndName

testNoID = TestCase $ Just "node0" @=? elementVarName (head $ assignNames [nodeNoID])

tests = hUnitTestToTests $ TestList [
  testSimpleNaming,
  testRepeatedNames,
  testNoID
  ]
