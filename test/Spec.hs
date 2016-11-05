import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Parse.TemplateParserTest as TPT
import qualified Naming.VariableAssignmentTest as VAT

main :: IO ()
main = defaultMain (TPT.tests ++ VAT.tests)
