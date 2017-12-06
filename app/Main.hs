module Main where

import System.Environment
import Data.Text (Text)
import qualified Data.Text as T

import Scheme.Parse
import Scheme.Eval
import Scheme.Error

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
