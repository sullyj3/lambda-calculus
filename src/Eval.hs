module Eval where

-- TODO these should be in seperate module
import Lib (Expr (..), Variable (..), Abstraction(..))

eval :: Expr -> Expr
eval =
  \case
    Abs (Abstraction v body) -> tryEtaReduce $ Abstraction v (eval body)
    v@(Var _) -> v
    Application fn arg ->
      let fn' = eval fn
          arg' = eval arg
       in case fn' of
            Abs (Abstraction paramVar body) ->
              eval $ betaReduce arg' paramVar body
            _ -> Application fn' arg'

-- TODO refactor to use these
newtype Argument = Argument Expr

newtype Body = Body Expr

betaReduce :: Expr -> Variable -> Expr -> Expr
betaReduce arg paramVar body =
  let replaceIn = betaReduce arg paramVar
   in case body of
        abstr@(Abs (Abstraction innerParamVar innerBody))
          -- The inner abstraction shadows the argument we are substituting,
          -- so we should not continue substituting in its body
          | paramVar == innerParamVar -> abstr
          | otherwise -> Abs (Abstraction innerParamVar (replaceIn innerBody))
        Var v
          | paramVar == v -> arg
          | otherwise -> Var v
        Application fn arg' -> Application (replaceIn fn) (replaceIn arg')

tryEtaReduce :: Abstraction -> Expr
tryEtaReduce abstr@(Abstraction v body) = case body of
  -- Can't eta reduce if the function we want to reduce to is the same as the bound variable,
  -- eg \x.xx
  Application (Var fnvar) _
    | v == fnvar -> Abs abstr
  Application fn (Var v')
    | v == v' -> fn
  _ -> Abs abstr
