module Main where

import System.Environment
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad

import Scheme.Parse
import Scheme.Eval
import Scheme.Error
import Scheme.REPL

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ args !! 0
    _ -> putStrLn "very-nice: only takes 1 argument"
