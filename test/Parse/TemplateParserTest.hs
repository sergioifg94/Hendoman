module Parse.TemplateParserTest(tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Parse.TemplateParser
import Tree.TemplateExpression

testIncorrect1 = TestCase (assertEqual "Incorrect text" Nothing (parseText "incorrect {{input"))

testIncorrect2 = TestCase (assertEqual "Incorrect text" Nothing (parseText "incorrect}} input"))

testCorrect1 = TestCase (assertEqual "Correct text" (Just [Literal "foo ", Template "bar"]) (parseText "foo {{bar}}"))

testCorrect2 = TestCase (assertEqual "Correct text" (Just [Literal "literal"]) (parseText "literal"))

testCorrect3 = TestCase (assertEqual "Correct text" (Just [Template "template"]) (parseText "{{template}}"))

tests = hUnitTestToTests $ TestList [
  TestLabel "testIncorrect1" testIncorrect1,
  TestLabel "testIncorrect2" testIncorrect2,
  TestLabel "testCorrect1" testCorrect1,
  TestLabel "testCorrect2" testCorrect2,
  TestLabel "testCorrect3" testCorrect3
  ]
