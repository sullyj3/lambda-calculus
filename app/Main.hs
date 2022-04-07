module Main where

import Lib
import Text.Megaparsec (parse)
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)

main :: IO ()
main = let

  prompt :: Text -> IO Text
  prompt s = do
    T.putStr s
    hFlush stdout
    T.getLine

  loop = do
    line <- prompt "> "
    case parse expr "interpreter" line of 
      Right e  -> print e
      Left err -> T.putStrLn "Parse failed"
    loop

  in do
    T.putStrLn "Lambda calculus interpreter"
    loop
