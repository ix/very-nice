{-# LANGUAGE OverloadedStrings #-}

module Scheme.Parse
  ( readExpr
  , parseString )
  where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Char
import Data.Text (Text)
import Data.List
import qualified Data.Text as T
import Data.Ratio
import Data.Complex
import qualified Data.Vector as V
import Numeric

import Scheme.LispVal

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ T.pack ("No match: " ++ show err)
  Right val -> val

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  _ <- char '"'
  return . String . T.pack $ x

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

parseCharacter :: Parser LispVal
parseCharacter = do
  _ <- try $ string "#\\"
  value <- try (caseInsensitiveString "newline" <|> caseInsensitiveString "space")
           <|> do x <- anyChar
                  ; notFollowedBy alphaNum
                  return [x]
  return $ Character $ case value of
                         "space" -> ' '
                         "newline" -> '\n'
                         _ -> (value !! 0)

escapedChars :: Parser Char
escapedChars = do _ <- char '\\'      -- backslash
                  x <- oneOf "\\\nrt" -- backslash or dquote
                  return $ case x of
                             '\\' -> x
                             '"'  -> x
                             'n'  -> '\n'
                             'r'  -> '\r'
                             't'  -> '\t'
                             _    -> x -- unreachable

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = T.pack $ first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> (return (Bool False)))

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble _ = error "Not float/num"

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal)
                  _ <- char '+'
                  y <- (try parseFloat <|> parseDecimal)
                  _ <- char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                _ <- char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                _ <- char '.'
                y <- many1 digit
                return $ Float (fst.head $ readFloat (x ++ "." ++ y))

parseNumber :: Parser LispVal
parseNumber = parseDecimal
          <|> parseDecPref
          <|> parseHex
          <|> parseOct
          <|> parseBin

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= return . Number . read

parseDecPref :: Parser LispVal
parseDecPref = do _ <- try $ string "#d"
                  x <- many1 digit
                  (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do _ <- try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hexToDec . T.pack $ x)

parseOct :: Parser LispVal
parseOct = do _ <- try $ string "#o"
              x <- many1 octDigit
              return $ Number (octToDec . T.pack $ x)

parseBin :: Parser LispVal
parseBin = do _ <- try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (binToDec . T.pack $ x)

hexToDec :: Text -> Integer
hexToDec x = fst $ readHex (T.unpack x) !! 0
octToDec :: Text -> Integer
octToDec x = fst $ readOct (T.unpack x) !! 0
binToDec :: Text -> Integer
binToDec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i . T.unpack
  where c2i c = if c == '0'
                then 0
                else 1

parseList :: Parser LispVal
parseList = between beg end parseList'
            where beg = (char '(' >> skipMany space)
                  end = (skipMany space >> char ')')

parseList' :: Parser LispVal
parseList' = do list <- sepEndBy parseExpr spaces
                maybeDatum <- optionMaybe (char '.' >> spaces >> parseExpr)
                return $ case maybeDatum of
                           Nothing -> List list
                           Just datum -> DottedList list datum

parseVector :: Parser LispVal
parseVector = between beg end parseVector'
              where beg = (string "#(" >> skipMany space)
                    end = (skipMany space >> char ')')

parseVector' = do list <- sepEndBy parseExpr spaces
                  return . Vector $ V.fromList list

parseQuoted :: Parser LispVal
parseQuoted = do _ <- char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do _ <- char '`'
                     x <- parseExpr
                     return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do _ <- char ','
                  x <- parseExpr
                  return $ List [Atom "unquote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuoted
        <|> parseQuasiquote
        <|> parseUnquote
        <|> try parseVector
        <|> try parseList

unwordsList :: [LispVal] -> Text
unwordsList = T.unwords . map showVal
