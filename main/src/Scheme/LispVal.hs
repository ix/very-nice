{-# LANGUAGE OverloadedStrings #-}

-- TODO:
-- - rethink adt

module Scheme.LispVal
  ( LispVal (..)
  , showVal )
  where

import Data.Text (Text)
import Data.Text as T
import Data.Ratio
import Data.Complex
import qualified Data.Vector as V

data LispVal =
    Atom Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector (V.Vector LispVal)
  | Number Integer
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)
  | String Text
  | Character Char
  | Bool Bool

instance Show LispVal where show = T.unpack . showVal

showVal :: LispVal -> Text
showVal (String contents) = "\"" `T.append` contents `T.append` "\""
showVal (Atom name) = name
showVal (Number contents) = T.pack $ show contents
showVal (Float contents) = T.pack $ show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" `T.append` unwordsList contents `T.append` ")"
showVal (DottedList head tail) = "(" `T.append` unwordsList head `T.append` " . " `T.append` showVal tail `T.append` ")"

-- AAAAAAAAA
unwordsList :: [LispVal] -> Text
unwordsList = T.unwords . fmap showVal
