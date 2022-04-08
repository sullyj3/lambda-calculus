import Test.Hspec
import Lib
import Eval

import Text.Megaparsec
import Data.Text (Text)

parsesAs :: (Show a, Eq a) => Parser a -> Text -> a -> Expectation
parsesAs parser input expected =
  parseMaybe parser input `shouldBe` Just expected

main :: IO ()
main = hspec do
  testExprParser
  testEvaluator

testExprParser :: SpecWith ()
testExprParser = describe "expression parser" do
  let exprParsesAs = parsesAs expr

  it "parses a single variable" do
    "a" `exprParsesAs` Var (Variable 'a')

  it "parses the identity function" do
    "\\x.x" `exprParsesAs` Abstraction (Variable 'x') (Var $ Variable 'x')

  it "parses an application of a variable to another" do
    "ab" `exprParsesAs` Application (Var $ Variable 'a') (Var $ Variable 'b')

  it "parses an application of the identity function to a variable" do
    let expected = Application (Var $ Variable 'a') (Var $ Variable 'b')
    "ab" `exprParsesAs` expected

  it "parses an application of an application" do
    let expected = Application (Application (Var $ Variable 'a') (Var $ Variable 'b')) (Var $ Variable 'c')
    "abc" `exprParsesAs` expected

  it "parses a single variable in parens" do
    let expected = Var $ Variable 'x'
    "(x)" `exprParsesAs` expected

  it "parses an application of a variable in parens to a variable" do
    let expected = Application (Var $ Variable 'x') (Var $ Variable 'y')
    "(x)y" `exprParsesAs` expected

  it "parses a triple application involving parens (1)" do
    let expected = Application (Application (Var $ Variable 'x') (Var $ Variable 'y')) (Var $ Variable 'z')
    "(x)y(z)" `exprParsesAs` expected

  it "parses an application of a variable to a parenthesized expression" do
    let expected = Application
                     (Var $ Variable 'e')
                     (Application (Var $ Variable 'f')
                                  (Var $ Variable 'g'))
    "e(fg)" `exprParsesAs` expected

parseUnsafeUnwrap :: Parser a -> Text -> a
parseUnsafeUnwrap p s = let 
  Just result = parseMaybe p s
  in result

reducesTo :: Text -> Text -> Expectation
reducesTo redex reduct = eval parsedRedex `shouldBe` parsedReduct 
  where
  parsedRedex = parseUnsafeUnwrap expr redex
  parsedReduct = parseUnsafeUnwrap expr reduct

testEvaluator :: SpecWith ()
testEvaluator = describe "evaluator" do
  it "beta reduces the identity function on a single variable" do
    "(\\x.x)a" `reducesTo` "a"

  it "beta reduces the identity function on itself" do
    "(\\x.x)(\\y.y)" `reducesTo` "(\\y.y)"

  it "does not substitute shadowed variables (1)" do
    "(\\x.(\\x.xa))z" `reducesTo` "(\\x.xa)"

  it "does not substitute shadowed variables (2)" do
    "(\\x.(\\x.xa)x)z" `reducesTo` "za"

  it "evaluates application of application to variable (1)" do
    "(ab)c" `reducesTo` "abc"

  it "evaluates application of application to variable (2)" do
    "((\\x.x)b)c" `reducesTo` "bc"

  it "evaluates application of application to variable (3)" do
    "((\\x.x)(\\y.yy))c" `reducesTo` "cc"

  it "evaluates abstraction containing reducible body" do
    "\\x.(\\y.y)z" `reducesTo` "\\x.z"

  it "further reduces if possible when substituting an abstraction" do
    "(\\x.xz)(\\x.x)" `reducesTo` "z"

  it "eta reduces" do
    "\\x.ax" `reducesTo` "a"

  it "does not eta reduce when inner function is bound" do
    "\\x.xx" `reducesTo` "\\x.xx"
