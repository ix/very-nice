{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

-- TODO:
-- - refactor numericBinop to support non-Integer types

module Scheme.Eval
  ( showVal
  , eval
  , Env
  , nullEnv
  , liftThrows
  , runIOThrows )
  where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Complex
import Data.Ratio
import Control.Monad.Except
import Data.IORef
import Control.Applicative

import Scheme.LispVal
import Scheme.Error

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)
type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispError IO

-- THE. EVALUATOR.
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env $ T.unpack id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred1, conseq, alt]) =
  do result <- eval env pred1
     case result of
       Bool False -> eval env alt
       Bool True  -> eval env conseq
       notBool    -> throwError $ TypeMismatch "boolean" notBool
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env (T.unpack var)
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env (T.unpack var)
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply (T.unpack func)
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

-- mutability
nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows act = runExceptT (trapError act) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Attempting to get unbound variable" var)
                         (liftIO . readIORef)
                         (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Attempting to get unbound variable" var)
                               (liftIO . (flip writeIORef value))
                               (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do valueRef <- newIORef value
                     env <- readIORef envRef
                     _ <- writeIORef envRef ((var, valueRef):env)
                     return value

-- list of primitives implemented
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eq?", eqv)
             , ("eqv?", eqv)
             , ("equal?", equal)

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

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals uno dos (AnyUnpacker unpacker) =
  do unpackuno <- unpacker uno
     unpackdos <- unpacker dos
     return $ unpackuno == unpackdos
  `catchError` (const $ return False)

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
                                               (all eqvPair $ zip uno dos) 
  where eqvPair (x1, x2) = case eqv [x1, x2] of
          Left err -> False
          Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [uno, dos] = do
  primEq <- or <$> mapM (unpackEquals uno dos)
                       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEq  <- eqv [uno, dos]
  return $ Bool (primEq || let (Bool x) = eqvEq in x)
equal badArgList = throwError $ NumArgs 2 badArgList
