module Lib where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T

type Parser = Parsec Void Text

newtype Variable = Variable Char
  deriving (Show, Eq)

data Expr
  = Abstraction Variable Expr
  | Var Variable
  | Application Expr Expr
  deriving (Show, Eq)


display :: Expr -> Text
display
  = \case
      Abstraction param body -> "\\" <> displayVar param <> "." <> display body
      (Var v) -> displayVar v
      (Application fn arg) -> case fn of
        abstr@(Abstraction _ _) -> parenthesize (display abstr) <> display arg
        Var v -> case arg of
          Var u -> displayVar v <> displayVar u
          Abstraction _ _ -> displayVar v <> parenthesize (display arg)
          Application _ _ -> displayVar v <> parenthesize (display arg)

        (Application _ _) -> parenthesize (display fn) <> display arg

  where 
    displayVar :: Variable -> Text
    displayVar (Variable c) = T.singleton c

    parenthesize t = "(" <> t <> ")"

term :: Parser Expr
term =
  try (uncurry Abstraction <$> abstraction)
    <|> try (parens expr)
    <|> (Var <$> variable)

expr :: Parser Expr
expr = makeExprParser term [[InfixL (Application <$ string "")]]

parens :: Parser a -> Parser a
parens = between (single '(') (single ')')

abstraction :: Parser (Variable, Expr)
abstraction = do
  argument <-
    between (single '\\') (single '.') variable
  body <- expr
  pure (argument, body)

application :: Parser (Expr, Expr)
application = do
  fn <- expr
  arg <- expr
  pure (fn, arg)

variable :: Parser Variable
variable = Variable <$> satisfy (`elem` ['a' .. 'z'])
