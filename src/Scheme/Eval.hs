{-# LANGUAGE OverloadedStrings #-}

-- TODO:
-- - refactor numericBinop to support non-Integer types

module Scheme.Eval
  ( showVal
  , eval )
  where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Complex
import Data.Ratio
import Control.Monad.Except

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
eval (List [Atom "if", pred1, conseq, alt]) =
  do result <- eval pred1
     case result of
       Bool False -> eval alt
       _ -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply (T.unpack func)
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("car", car)
             , ("cdr", cdr)

             , ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)

             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("/=", numBoolBinop (/=))
             , ("<=", numBoolBinop (<=))
             , (">=", numBoolBinop (>=))
             , ("and", boolBoolBinop (&&))
             , ("or", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))

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

-- operator unpackers
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return (f v)

-- ast unpackers
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError Text
unpackStr (String s) = return s
unpackStr (Number s) = return $ T.pack $ show s
unpackStr (Bool s) = return $ T.pack $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

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

--- numerals
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

-- list primitives
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1] -- lists are Nil terminated
cons [x, List xs] = return $ List $ x:xs
cons [x, DottedList xs xl] = return $ DottedList (x:xs) xl
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- equality checkers
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool uno), (Bool dos)] = return $ Bool $ uno == dos
eqv [(Number uno), (Number dos)] = return $ Bool $ uno == dos
eqv [(String uno), (String dos)] = return $ Bool $ uno == dos
eqv [(Atom uno), (Atom dos)] = return $ Bool $ uno == dos
eqv [(DottedList xs x), (DottedList ys y)] = eqv $ [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List uno), (List dos)] = return $ Bool $ (length uno == length dos) &&
                                               (all eqvPair $ zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
          Left err -> False
          Right (Bool val) -> val = return $ Bool False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList
