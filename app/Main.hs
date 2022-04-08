module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)

import Eval (eval)
import Expr (display)
import Parse (parseExpr)

main :: IO ()
main =
  let prompt :: Text -> IO Text
      prompt s = do
        T.putStr s
        hFlush stdout
        T.getLine

      loop = do
        line <- prompt "λ > "
        case parseExpr "interpreter" line of
          Right e -> T.putStrLn . display . eval $ e
          Left err -> T.putStrLn $ "Parse failed: " <> T.pack (show err)
        loop
   in do
        T.putStrLn "Lambda calculus interpreter"
        loop
