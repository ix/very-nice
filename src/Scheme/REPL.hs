{-# LANGUAGE OverloadedStrings #-}

module Scheme.REPL
  ( runRepl
  , evalAndPrint )
  where

-- TODO: readline/bksp w/out rlwrap
--       overall this is really simplistic and needs work bad

import System.IO
import Control.Monad.Except

import Scheme.Error
import Scheme.Eval
import Scheme.Parse

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred' prompt action = do
  result <- prompt
  if pred' result
    then return ()
    else action result >> until_ pred' prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "verynice>>> ") evalAndPrint
