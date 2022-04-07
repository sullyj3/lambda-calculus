import Test.Hspec
import Lib

import Text.Megaparsec

parserSucceeds parser input expected =
  parseMaybe parser input `shouldBe` Just expected

main :: IO ()
main = hspec do
    
  describe "expression parser" do
    let exprSucceeds = parserSucceeds expr
          
    it "parses a single variable" do
      "a" `exprSucceeds` (Var $ Variable 'a')

    it "parses the identity function" do
      "\\x.x" `exprSucceeds` Abstraction (Variable 'x') (Var $ Variable 'x')

    it "parses an application of a variable to another" do
      "ab" `exprSucceeds` Application (Var $ Variable 'a') (Var $ Variable 'b')

    it "parses an application of the identity function to a variable" do
      let expected = Application (Var $ Variable 'a') (Var $ Variable 'b')
      "ab" `exprSucceeds` expected

    it "parses an application of an application" do
      let expected = Application (Application (Var $ Variable 'a') (Var $ Variable 'b')) (Var $ Variable 'c')
      "abc" `exprSucceeds` expected

    it "parses a single variable in parens" do
      let expected = Var $ Variable 'x'
      "(x)" `exprSucceeds` expected

    it "parses an application of a variable in parens to a variable" do
      let expected = Application (Var $ Variable 'x') (Var $ Variable 'y')
      "(x)y" `exprSucceeds` expected

    it "parses a triple application involving parens (1)" do
      let expected = Application (Application (Var $ Variable 'x') (Var $ Variable 'y')) (Var $ Variable 'z')
      "(x)y(z)" `exprSucceeds` expected

    it "parses an application of a variable to a parenthesized expression" do
      let expected = Application 
                       (Var $ Variable 'e')
                       (Application (Var $ Variable 'f') 
                                    (Var $ Variable 'g')) 
      "e(fg)" `exprSucceeds` expected
