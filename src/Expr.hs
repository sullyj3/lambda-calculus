module Expr where

import Data.Text (Text)
import qualified Data.Text as T

newtype Variable = Variable Char
  deriving (Show, Eq)

data Application = Application
  { appFn :: Expr,
    appArg :: Expr
  }
  deriving (Show, Eq)

data Abstraction = Abstraction
  { abstrParam :: Variable,
    abstrBody :: Expr
  }
  deriving (Show, Eq)

data Expr
  = Var Variable
  | App Application
  | Abs Abstraction
  deriving (Show, Eq)

display :: Expr -> Text
display =
  \case
    Var v -> displayVar v
    App app -> displayApplication app
    Abs abstr -> displayAbstraction abstr
  where
    displayVar (Variable c) = T.singleton c

    displayAbstraction (Abstraction param body) =
      "\\" <> displayVar param <> "." <> display body

    displayApplication (Application fn arg) = case fn of
      Var v -> case arg of
        Var u -> displayVar v <> displayVar u
        App (Application _ _) -> displayVar v <> parenthesize (display arg)
        Abs _ -> displayVar v <> parenthesize (display arg)
      App (Application _ _) -> parenthesize (display fn) <> display arg
      Abs abstr -> parenthesize (displayAbstraction abstr) <> display arg

    parenthesize t = "(" <> t <> ")"
