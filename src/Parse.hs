module Parse where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Data.Void (Void)
import Expr
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

variable :: Parser Variable
variable = Variable <$> satisfy (`elem` ['a' .. 'z'])

application :: Parser (Expr -> Expr -> Expr)
application = (\e1 e2 -> App (Application e1 e2)) <$ string ""

abstraction :: Parser Abstraction
abstraction = do
  argument <-
    between (single '\\') (single '.') variable
  body <- expr
  pure $ Abstraction argument body

parens :: Parser a -> Parser a
parens = between (single '(') (single ')')

term :: Parser Expr
term =
  try (Abs <$> abstraction)
    <|> try (parens expr)
    <|> (Var <$> variable)

expr :: Parser Expr
expr = makeExprParser term [[InfixL application]]

parseUnsafeUnwrap :: Parser a -> Text -> a
parseUnsafeUnwrap p s =
  let Just result = parseMaybe p s
   in result

parseExpr :: String -> Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse expr
