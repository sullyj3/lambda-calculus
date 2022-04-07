module Lib where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

newtype Variable = Variable Char
  deriving (Show, Eq)

data Expr
  = Abstraction Variable Expr
  | Var Variable
  | Application Expr Expr
  deriving (Show, Eq)

term =
  try (uncurry Abstraction <$> abstraction)
    <|> try (parens expr)
    <|> (Var <$> variable)

expr :: Parser Expr
expr = makeExprParser term [[InfixL (Application <$ string "")]]

parens = between (single '(') (single ')')

abstraction :: Parser (Variable, Expr)
abstraction = do
  single '\\'
  argument <- variable
  single '.'
  body <- expr
  pure (argument, body)

application :: Parser (Expr, Expr)
application = do
  fn <- expr
  arg <- expr
  pure (fn, arg)

variable :: Parser Variable
variable = Variable <$> satisfy (`elem` ['a' .. 'z'])
