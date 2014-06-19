-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import System.Environment


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " arguments; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseError) = "Parse error at " ++ show parseError
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (UnboundVar message varname) = message ++ ": " ++ varname
instance Show LispError where show = showError
instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val


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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  list <- endBy parseExpr spaces
  char '.'
  spaces
  val <- parseExpr
  return $ DottedList list val

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do
               char '('
               list <- try parseList <|> parseDottedList
               char ')'
               return list



showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ showVal x ++ ")"
showVal (Number n) = show n
showVal (String str) = "\"" ++ str ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm



apply :: String -> [LispVal] -> ThrowsError LispVal
apply fn args = maybe (throwError $ NotFunction "Unrecognized primitive function" fn) ($ args) $ lookup fn primitives

primitives :: [ (String, [LispVal] -> ThrowsError LispVal) ]
primitives = [
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ( "+", numericBinop (+) ),
    ( "-", numericBinop (-) ),
    ( "*", numericBinop (*) ),
    ( "/", numericBinop (div) ),
    ( "mod", numericBinop mod ),
    ( "quotient", numericBinop quot ),
    ( "remainder", numericBinop rem ) ]


first :: [a] -> a
first = head
second :: [a] -> a
second = head . tail

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpack op args = if length args < 2
                             then throwError $ NumArgs 2 args
                             -- else mapM unpack args >>= return . Bool . foldl1 op
                             else do left <- unpack $ first args
                                     right <- unpack $ second args
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op val@[_] = throwError $ NumArgs 2 val
numericBinop op args = mapM unpackNum args >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number a) = return a
unpackNum (String s) = let parsed = reads s in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String s
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum


unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool s) = return s
unpackBool notBool = throwError $ TypeMismatch "bool" notBool


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
