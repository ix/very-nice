{-# LANGUAGE OverloadedStrings #-}

-- TODO:
-- - refactor numericBinop to support non-Integer types
-- - function definitions

module Scheme.Eval
  ( showVal
  , eval )
  where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Complex
import Data.Ratio

import Scheme.LispVal
import Scheme.Error

unwordsList :: [LispVal] -> Text
unwordsList = T.unwords . map showVal

eval :: LispVal -> ThrowsError LispVal
-- self-evaluating
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: Text -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive functino args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(Text, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)

             , ("boolean?", unaryOp boolp)
             , ("pair?", unaryOp pairp)
             , ("list?", unaryOp listp)
             , ("symbol?", unaryOp symbolp)
             , ("char?", unaryOp charp)
             , ("string?", unaryOp stringp)
             , ("vector?", unaryOp vectorp)

             , ("number?", unaryOp numberp)
             , ("complex?", unaryOp complexp)
             , ("real?", unaryOp realp)
             , ("rational?", unaryOp rationalp)
             , ("integer?", unaryOp integerp)]

numericBinop :: (Num a) => (a -> a -> a) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwsError $ NumArgs 2 []
numericBinop op singleval@[_] = throwsError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

-- TODO
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v 

-- type testing
symbolp :: LispVal -> LispVal
pairp   :: LispVal -> LispVal
listp   :: LispVal -> LispVal
boolp   :: LispVal -> LispVal
charp   :: LispVal -> LispVal
stringp :: LispVal -> LispVal
vectorp :: LispVal -> LispVal

symbolp (Atom _) = Bool True
symbolp _ = Bool False
pairp (List _) = Bool True
pairp (DottedList _ _) = Bool True
pairp _  = Bool False
listp (List _) = Bool True
listp _ = Bool False
boolp (Bool _) = Bool True
boolp _ = Bool False
charp (Character _) = Bool True
charp _ = Bool False
stringp (String _) = Bool True
stringp _ = Bool False
vectorp (Vector _) = Bool True
vectorp _ = Bool False

-- numerals
numberp   :: LispVal -> LispVal
complexp  :: LispVal -> LispVal
realp     :: LispVal -> LispVal
rationalp :: LispVal -> LispVal
integerp  :: LispVal -> LispVal

numberp = complexp
complexp (Complex _) = Bool True
complexp x = realp x
realp (Float _) = Bool True
realp x = rationalp x
rationalp (Ratio x) = Bool True
rationalp x = integerp x
integerp (Number x) = Bool True
integerp _ = Bool False
