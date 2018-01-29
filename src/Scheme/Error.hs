{-# LANGUAGE OverloadedStrings #-}

-- TODO: Use Control.Monad.Except, and Either types for Parsec

module Scheme.Error
  ( LispError (..)
  , ThrowsError
  , trapError
  , extractValue )
  where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec

import Scheme.LispVal

data LispError =
    NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

type ThrowsError = Either LispError

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                     ++ "args; found values " ++ T.unpack (unwordsList found)
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ " found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError _ = "Something happened"

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
-- purposely undefined for Left constructor; programmer error

-- don't really want to redeclare this but fugit quick fix
unwordsList :: [LispVal] -> Text
unwordsList = T.unwords . map showVal
