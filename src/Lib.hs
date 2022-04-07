module Lib where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Text (Text)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))

type Parser = Parsec Void Text

data Variable = Variable Char
  deriving (Show, Eq)


data Expr = Abstraction Variable Expr
          | Var Variable
          | Application Expr Expr
  deriving (Show, Eq)
      
term =
  try (uncurry Abstraction <$> abstraction) <|>
  try (parens expr) <|>
  (Var <$> variable)

expr :: Parser Expr
expr = makeExprParser term [[ InfixL (Application <$ string "")]]


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
variable = Variable <$> satisfy (`elem` ['a'..'z'])


