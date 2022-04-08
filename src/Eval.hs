module Eval where

-- TODO these should be in seperate module
import Lib

eval :: Expr -> Expr
eval =
  \case
    v@(Var _) -> v
    App (Application fn arg) -> tryBetaReduce $ Application (eval fn) (eval arg)
    Abs (Abstraction v body) -> tryEtaReduce $ Abstraction v (eval body)

-- TODO refactor to use these
newtype Argument = Argument Expr

newtype Body = Body Expr

tryBetaReduce :: Application -> Expr
tryBetaReduce = \case
  Application (Abs (Abstraction paramVar body)) arg ->
    eval $ betaReduce arg paramVar body
  app -> App app

betaReduce :: Expr -> Variable -> Expr -> Expr
betaReduce arg paramVar body =
  let replaceIn = betaReduce arg paramVar
   in case body of
        Var v
          -- Found an occurrence, replace it
          | paramVar == v -> arg
          | otherwise -> Var v
        App (Application fn arg') -> App $ Application (replaceIn fn) (replaceIn arg')
        abstr@(Abs (Abstraction innerParamVar innerBody))
          -- The inner abstraction shadows the argument we are substituting,
          -- so we should not continue substituting in its body
          | paramVar == innerParamVar -> abstr
          | otherwise -> Abs (Abstraction innerParamVar (replaceIn innerBody))

tryEtaReduce :: Abstraction -> Expr
tryEtaReduce abstr@(Abstraction v body) = case body of
  -- Can't eta reduce if the function we want to reduce to is the same as the bound variable,
  -- eg \x.xx
  App (Application (Var fnvar) _)
    | v == fnvar -> Abs abstr
  App (Application fn (Var v'))
    | v == v' -> fn
  _ -> Abs abstr
