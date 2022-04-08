module Eval where

-- TODO these should be in seperate module
import Lib (Expr (..), Variable (..))

eval :: Expr -> Expr
eval =
  \case
    (Abstraction v body) -> Abstraction v (eval body)
    v@(Var _) -> v
    Application fn arg ->
      let fn' = eval fn
          arg' = eval arg
       in case fn' of
            Abstraction paramVar body ->
              eval $ betaReduce arg' paramVar body
            _ -> Application fn' arg'

-- TODO refactor to use these
newtype Argument = Argument Expr

newtype Body = Body Expr

betaReduce :: Expr -> Variable -> Expr -> Expr
betaReduce arg paramVar =
  let go = betaReduce arg paramVar
   in \case
        abstr@(Abstraction innerParamVar body)
          -- The inner abstraction shadows the argument we are substituting,
          -- so we should not continue substituting in its body
          | paramVar == innerParamVar -> abstr
          | otherwise -> Abstraction innerParamVar (go body)
        Var v
          | paramVar == v -> arg
          | otherwise -> (Var v)
        Application fn arg' -> Application (go fn) (go arg')
