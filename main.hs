-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

stringFragment :: Parser Char
stringFragment = (string "\\\"" >> return '"')
              <|> (string "\\\\" >> return '\\')
              <|> (string "\\n" >> return '\n')
              <|> (string "\\r" >> return '\r')
              <|> (string "\\t" >> return '\t')
              <|> noneOf "\""

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ stringFragment
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> symbol <|> digit)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do
--   x <- many1 digit
--   let num = read x
--   return $ Number num

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value " ++ show val


main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ args !! 0
