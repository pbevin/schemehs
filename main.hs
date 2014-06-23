{-# LANGUAGE ExistentialQuantification #-}

-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Data.IORef
import System.Environment
import System.IO
import System.Console.Readline


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func { params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do result <- eval env pred
     case result of
       Bool False -> eval env alt
       _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env
apply (IOFunc func) args = func args

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func:args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

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
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal", equal),
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop (div)),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem) ]

ioPrimitives :: [ (String, [LispVal] -> IOThrowsError LispVal) ]
ioPrimitives = [
    ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll) ]



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


car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] y] = return y
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool a1, Bool a2] = return $ Bool $ a1 == a2
eqv [Number a1, Number a2] = return $ Bool $ a1 == a2
eqv [String a1, String a2] = return $ Bool $ a1 == a2
eqv [Atom a1, Atom a2] = return $ Bool $ a1 == a2
eqv [DottedList xs x, DottedList ys y] = eqv [List (x:xs), List (y:ys)]
eqv [List xs, List ys] = return $ Bool $ (length xs == length ys) && (all eqvPair $ zip xs ys)
        where eqvPair (x, y) = case eqv [x, y] of
                                 Left err -> False
                                 Right (Bool val) -> val
eqv [_, _] = return $ Bool $ False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a1 a2 (AnyUnpacker unpacker) =
  do u1 <- unpacker a1
     u2 <- unpacker a2
     return $ u1 == u2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [a1, a2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals a1 a2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [a1, a2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList



type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >>= return
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef

extendEnv :: [(String, LispVal)] -> [(String, IORef LispVal)] -> IO [(String, IORef LispVal)]
extendEnv bindings env = liftM (++ env) $ mapM addBinding bindings
  where addBinding (name, value) = do ref <- newIORef value
                                      return (name, ref)






readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

load :: String -> IOThrowsError [LispVal]
load file = (liftIO $ readFile file) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String file] = do
  exprs <- load file
  return $ List exprs

-- readExpr :: String -> ThrowsError LispVal
-- readExpr input = case parse parseExpr "lisp" input of
--   Left err -> throwError $ Parser err
--   Right val -> return val


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
-- readPrompt prompt = flushStr prompt >> getLine
readPrompt prompt = do
  line <- readline prompt
  case line of
    Nothing -> return "quit"
    Just x -> addHistory x >> return x

evalString :: Env -> String -> IO String
-- evalString env expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

primitiveBindings :: IO (Env)
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) primitives
                                               ++ map (makeFunc IOFunc) ioPrimitives)
  where makeFunc constructor (var, func) = (var, constructor func)

-- runOne :: String -> IO ()
-- runOne expr = primitiveBindings >>= flip evalAndPrint expr
runOne :: [String] -> IO ()
runOne (file:args) =
  let lispArgs = List $ map String args
      cmd = List [Atom "load", String file]
      in do
        env <- primitiveBindings >>= flip bindVars [("args", lispArgs)]
        (runIOThrows $ liftM show $ eval env cmd) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "λ>>> ") . evalAndPrint

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne args
