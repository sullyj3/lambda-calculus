module Lib where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

newtype Variable = Variable Char
  deriving (Show, Eq)

data Abstraction = Abstraction
  { abstrParam :: Variable,
    abstrBody :: Expr
  }
  deriving (Show, Eq)

data Application = Application
  { appFn :: Expr,
    appArg :: Expr
  }
  deriving (Show, Eq)

data Expr
  = Abs Abstraction
  | Var Variable
  | App Application
  deriving (Show, Eq)

display :: Expr -> Text
display =
  \case
    Abs abstr -> displayAbstraction abstr
    Var v -> displayVar v
    App app -> displayApplication app
  where
    displayVar (Variable c) = T.singleton c

    displayAbstraction (Abstraction param body) =
      "\\" <> displayVar param <> "." <> display body

    displayApplication (Application fn arg) = case fn of
      Abs abstr -> parenthesize (displayAbstraction abstr) <> display arg
      Var v -> case arg of
        Var u -> displayVar v <> displayVar u
        Abs _ -> displayVar v <> parenthesize (display arg)
        App (Application _ _) -> displayVar v <> parenthesize (display arg)
      App (Application _ _) -> parenthesize (display fn) <> display arg

    parenthesize t = "(" <> t <> ")"

term :: Parser Expr
term =
  try (Abs <$> abstraction)
    <|> try (parens expr)
    <|> (Var <$> variable)

expr :: Parser Expr
expr = makeExprParser term [[InfixL application]]

application :: Parser (Expr -> Expr -> Expr)
application = (\e1 e2 -> App (Application e1 e2)) <$ string ""

parens :: Parser a -> Parser a
parens = between (single '(') (single ')')

abstraction :: Parser Abstraction
abstraction = do
  argument <-
    between (single '\\') (single '.') variable
  body <- expr
  pure $ Abstraction argument body

variable :: Parser Variable
variable = Variable <$> satisfy (`elem` ['a' .. 'z'])
