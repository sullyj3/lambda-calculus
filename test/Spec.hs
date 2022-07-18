import Data.Text (Text)
import Eval (eval)
import qualified Expr
import Expr
  ( Abstraction (Abstraction),
    Application (Application),
    Expr (Abs, App, Var),
    Variable (Variable),
  )
import Parse (Parser, expr, parseUnsafeUnwrap)
import Test.Hspec
  ( Expectation,
    SpecWith,
    describe,
    hspec,
    it,
    shouldBe,
  )
import Text.Megaparsec (parseMaybe)

parsesAs :: (Show a, Eq a) => Parser a -> Text -> a -> Expectation
parsesAs parser input expected =
  parseMaybe parser input `shouldBe` Just expected

main :: IO ()
main = hspec do
  testExprParser
  testEvaluator
  testDisplay

testExprParser :: SpecWith ()
testExprParser = describe "expression parser" do
  let exprParsesAs = parsesAs expr

  it "parses a single variable" do
    "a" `exprParsesAs` Var (Variable 'a')

  it "parses the identity function" do
    "\\x.x" `exprParsesAs` Abs (Abstraction (Variable 'x') (Var $ Variable 'x'))

  it "parses an application of a variable to another" do
    "ab" `exprParsesAs` App (Application (Var $ Variable 'a') (Var $ Variable 'b'))

  it "parses an application of the identity function to a variable" do
    let expected = App $ Application (Var $ Variable 'a') (Var $ Variable 'b')
    "ab" `exprParsesAs` expected

  it "parses an application of an application" do
    let expected = App $ Application (App $ Application (Var $ Variable 'a') (Var $ Variable 'b')) (Var $ Variable 'c')
    "abc" `exprParsesAs` expected

  it "parses a single variable in parens" do
    let expected = Var $ Variable 'x'
    "(x)" `exprParsesAs` expected

  it "parses an application of a variable in parens to a variable" do
    let expected = App $ Application (Var $ Variable 'x') (Var $ Variable 'y')
    "(x)y" `exprParsesAs` expected

  it "parses a triple application involving parens (1)" do
    let expected = App $ Application (App $ Application (Var $ Variable 'x') (Var $ Variable 'y')) (Var $ Variable 'z')
    "(x)y(z)" `exprParsesAs` expected

  it "parses an application of a variable to a parenthesized expression" do
    let expected =
          App $
            Application
              (Var $ Variable 'e')
              ( App $
                  Application
                    (Var $ Variable 'f')
                    (Var $ Variable 'g')
              )
    "e(fg)" `exprParsesAs` expected

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

  it "evaluates a chain of eta reductions" do
    "\\y.(\\a.ba)(\\x.yx)" `reducesTo` "b"

testDisplay = describe "Expr.display" do
  it "displays application to application correctly" do
    let e = App $ Application 
              (App $ Application (Var $ Variable 'a') (Var $ Variable 'b'))
              (App $ Application (Var $ Variable 'x') (Var $ Variable 'y'))
    Expr.display e `shouldBe` "(ab)(xy)"
