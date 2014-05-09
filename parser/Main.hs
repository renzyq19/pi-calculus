{-# LANGUAGE TupleSections #-}
module Main where

import Control.Arrow (second)
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Monad (liftM, liftM2, unless)
import Control.Monad.Error (throwError)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), catchE, runExceptT, throwE)
import Data.IORef (IORef, newIORef, readIORef,writeIORef)
import Data.List (intercalate)
import Data.Maybe (isJust)
import System.Environment (getArgs, getProgName)
import System.IO (Handle, IOMode(..), hFlush, hGetLine, hPutStrLn,openFile, stderr, stdin, stdout)
import Text.ParserCombinators.Parsec

data PiProcess = Null
               | In   Term Term
               | Out  Term Term
               | New  Term
               | PiProcess `Seq`   PiProcess -- Sequential Composition
               | PiProcess `Conc`  PiProcess -- Parallel   Composition
               | Replicate PiProcess         -- Infinite parallel replication
               | Let Term Term PiProcess
               | If Condition PiProcess PiProcess
                 deriving (Eq)

data Term = TVar Name 
          | TStr String
          | TNum Integer
          | TFun Name [Term] Int
            deriving (Eq)

data Value = Proc PiProcess 
           | Term Term
           | Chan Channel
           | Func TermFun

data PiError = NumArgs Name Integer [Term]
             | TypeMismatch String [Value]
             | Parser ParseError
             | UnboundVar String String
             | NotTerm Name Value
             | NotFunction String String
             | NotChannel String
             | Default String

data Channel = Channel { 
               handle      :: Handle
             , chanType    :: Type
             , serialize   :: Value  -> String
             , deserialize :: String -> IOThrowsError Value
             }

type IOThrowsError = ExceptT PiError IO 
type ThrowsError   = Either  PiError

type Name      = String
type Type      = String
data Condition = Term `Equals` Term deriving (Eq)

type Env = IORef [(String , IORef Value)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var) $ readIORef envRef

getVar :: Env -> String -> IOThrowsError Value 
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwE $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> Value -> IOThrowsError Value
setVar envRef var val = do env <- liftIO $ readIORef envRef
                           maybe (throwE $ UnboundVar "Setting an unbound variable" var)
                                 (liftIO . flip writeIORef val)
                                 (lookup var env)
                           return val
                           
defineVar :: Env -> String -> Value -> IOThrowsError Value
defineVar envRef var val = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var val >> return val
        else liftIO $ do
            valueRef <- newIORef val
            env      <- readIORef envRef
            writeIORef envRef ((var,valueRef):env)
            return val

showValue :: Value -> String
showValue (Proc p)  = show p
showValue (Term t)  = show t
showValue (Chan c)  = chanType c
showValue (Func _)  = "Function" 

showPi :: PiProcess -> String
showPi Null = "0"
showPi (In c m) =  "in(" ++ show c ++ "," ++ show m ++ ")"
showPi (Out c m) =  "out(" ++ show c ++ "," ++  show m ++ ")"
showPi (Replicate proc) =  "!(" ++ show proc ++ ")"
showPi (p1 `Conc` p2) = show p1 ++ "|\n" ++ show p2
showPi (p1 `Seq` Null) = show p1
showPi (p1 `Seq` p2) = show p1 ++ ";\n" ++ show p2 
showPi (New n)   = "new " ++ show n
showPi (If c p1 Null) = "if " ++ show c ++ " then " ++ show p1 
showPi (If c p1 p2)   = "if " ++ show c ++ " then " ++ show p1 ++ " else " ++ show p2
showPi (Let n t p)    = "let " ++ show n ++ " = " ++ show t ++ " in\n" ++ show p

showTerm :: Term -> String
showTerm (TVar x)   = x
showTerm (TStr str) = str
showTerm (TNum num) = show num
showTerm (TFun n [] 0 ) = n ++ "()"
showTerm (TFun n ts _ ) = n ++ "(" ++ intercalate "," (map show ts) ++ ")"

showCond :: Condition -> String
showCond (t1 `Equals` t2) = show t1 ++ " == " ++ show t2

showError :: PiError -> String
showError (UnboundVar message var)      = message ++ ": " ++ var
showError (NotFunction message fun)     = message ++ ": " ++ fun
showError (NotChannel chan)             = "Not a channel: " ++ chan 
showError (NotTerm name var)            = "Expecting " ++ name ++ " to be a Term, found: " ++ show var
showError (NumArgs name expected found) = "Expected " ++ show name ++ show expected ++ " args; found values "
                                          ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found "
                                          ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default msg)                 = msg

instance Show PiProcess where show = showPi
instance Show Term      where show = showTerm
instance Show Condition where show = showCond
instance Show Value     where show = showValue
instance Show PiError   where show = showError

unwordsList :: [Term] -> String
unwordsList = unwords . map show

parseNull :: Parser PiProcess
parseNull = do
            paddedChar '0'
            return Null

parseIn :: Parser PiProcess
parseIn = do
            _ <- string "in("
            name <- parseTerm
            paddedComma
            var  <- parseTerm
            _ <- char ')'
            parseSeq $ In name var 

parseOut :: Parser PiProcess
parseOut = do
            _ <- string "out("
            name <- parseTerm
            paddedComma
            term  <- parseTerm
            _ <- char ')'
            parseSeq $ Out name term 

parseReplicate :: Parser PiProcess
parseReplicate = do
            _ <- string "!("
            process <- parseProcess
            _ <- char ')'
            return $ Replicate process

paddedChar :: Char ->  Parser ()
paddedChar ch = do
            spaces
            _ <- char ch
            spaces

paddedStr :: String -> Parser ()
paddedStr str = do
            spaces
            _ <- string str
            spaces

parseSeq :: PiProcess -> Parser PiProcess
parseSeq p1 = do
            p2 <- try (do {paddedChar ';' ; parseProcess}) <|> return Null
            return $ p1 `Seq` p2

parseNew :: Parser PiProcess
parseNew = do
            _ <- string "new"
            spaces
            name <- parseTerm
            parseSeq $ New name

parseIf :: Parser PiProcess
parseIf = do
            _ <- string "if" 
            spaces
            cond <- parseCondition
            paddedStr "then"
            p1 <- parseProcess
            p2 <- try (do {paddedStr "else" ; parseProcess}) <|> return Null
            return $ If cond p1 p2

parseLet :: Parser PiProcess
parseLet = do
            _ <- string "let"
            spaces
            name <- parseTerm
            paddedChar '='
            term <- parseTerm
            paddedStr "in"
            p    <- parseProcess
            return $ Let name term p

parseCondition :: Parser Condition
parseCondition = do
            t1 <- parseTerm
            paddedChar '='
            t2 <- parseTerm
            return $ t1 `Equals` t2

parseTVar :: Parser Term
parseTVar = liftM TVar readVar 

parseTFun :: Parser Term
parseTFun = do
            name <- readVar
            spaces
            args <- bracketed $ sepBy parseTerm paddedComma
            return $ TFun name args (length args) 

parseTStr :: Parser Term
parseTStr = do
        _ <- char '"'
        x <- many $ noneOf "\""
        _ <- char '"'
        return $ TStr x

parseTNum :: Parser Term
parseTNum = liftM (TNum . read) (many1 digit)
        
readVar :: Parser Name
readVar = do
            frst <- letter
            rest <- many $ letter <|> digit <|> symbol
            return $ frst:rest

symbol :: Parser Char
symbol = oneOf "'._"

paddedComma :: Parser ()
paddedComma = paddedChar ','

parseTerm :: Parser Term
parseTerm =  try parseTFun
         <|> parseTNum
         <|> parseTVar
         <|> parseTStr

parseProcess :: Parser PiProcess
parseProcess = liftM (foldr1 Conc) $ sepBy parseProcess' (paddedChar '|')
    where
    parseProcess'  = bracketed parseProcess'' <|> parseProcess''
    parseProcess'' = parseNull 
                 <|> try parseIf
                 <|> parseIn 
                 <|> parseOut
                 <|> parseReplicate
                 <|> parseNew
                 <|> parseLet

bracketed :: Parser a -> Parser a
bracketed parser = do
                    _ <- char '('
                    spaces
                    res <- parser
                    spaces
                    _ <- char ')'
                    return res

type TermFun = [Term] -> ThrowsError Term

bindVars :: Env -> [(String , Value)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where
        extendEnv bs env     = liftM (++ env) (mapM addBinding bs)
        addBinding (var,val) = liftM (var,) $ newIORef val

coreBindings :: IO Env
coreBindings = do
                n <- nullEnv 
                e1   <- bindVars n (map (second Func) primitives) 
                bindVars e1 (map (second Chan) nativeChannels)

stdStrChan :: Handle -> Channel
stdStrChan h = Channel h "string" show dSer
    where
        dSer str = liftM Term $ liftThrows $ readTerm str 

fileChan :: FilePath -> IOThrowsError Channel
fileChan file = do
                han <- liftIO $ openFile file ReadWriteMode
                return $ Channel han "string" show undefined 

nativeChannels :: [(String   , Channel)]
nativeChannels = [ ("stdin"  , stdStrChan stdin) 
                 , ("0"      , stdStrChan stdin) 
                 , ("stdout" , stdStrChan stdout)
                 , ("1"      , stdStrChan stdout)
                 , ("stderr" , stdStrChan stderr)
                 , ("2"      , stdStrChan stderr)
                 ]

primitives :: [(String      , TermFun)]
primitives = [ ("true"      , constId "true")
             , ("false"     , constId "false")
             , ("fst"       , first)
             , ("snd"       , secnd)
             , ("hash"      , unaryId "hash")
             , ("pk"        , unaryId "pk")
             , ("http"      , http)
             , ("getmsg"    , getmsg)
             , ("pair"      , binaryId "pair")
             , ("sdec"      , sdec)
             , ("senc"      , binaryId "senc")
             , ("adec"      , adec)
             , ("aenc"      , binaryId "aenc")
             , ("sign"      , binaryId "sign")
             , ("checksign" , checksign)
             , ("mac"       , binaryId "mac")
             ]

constId :: String -> TermFun
constId name [] = return $ TFun name [] 0
constId name e  = throwError $ NumArgs name 0 e

unaryId :: String -> TermFun
unaryId name [x] =  return $ TFun name [x] 1
unaryId name e  = throwError $ NumArgs name 1 e

binaryId :: String ->  TermFun
binaryId name [x,y] = return $ TFun name [x,y] 2 
binaryId name e  = throwError $ NumArgs name 2 e

getmsg :: TermFun
getmsg [TFun "sign" [_,y] 2] = return y
getmsg e = throwError $ TypeMismatch "sign" $ map Term e

first :: TermFun
first [TFun "pair" [x, _] 2] = return x
first e = throwError $ TypeMismatch "pair" $ map Term e 

secnd :: TermFun
secnd [TFun "pair" [_,y] 2] = return y
secnd e = throwError $ TypeMismatch "pair" $ map Term e 

http :: TermFun
http [TVar _] = undefined
http _        = undefined

sdec :: TermFun
sdec [k1, TFun "senc" [k2,y] 2]
    |k1 == k2  = return y
    |otherwise = throwError $ Default "keys not the same in sdec"
sdec e = throwError $ TypeMismatch "(var,senc(var,var))" $ map Term e

adec :: TermFun
adec [x , TFun "aenc" [TFun "pk" [k] 1, y ] 2]
    | x == k = return y
    | otherwise= throwError $ Default "keys not same in adec" 
adec e = throwError $ TypeMismatch "(var,aenc(pk(var),var))" $ map Term e

checksign :: TermFun
checksign [TFun "pk" [k1] 1 , TFun "sign" [k2,_] 2 ]
    | k1 == k2  = constId "true"  [] 
    | otherwise = constId "false" []
checksign e = throwError $ TypeMismatch "(pk(var),sign(var,var))" $ map Term e

main :: IO ()
main = do
        name <- getProgName
        args <- getArgs
        case args of
            []  -> runRepl
            [x] -> runProcess x
            _   -> do
                    putStrLn "Use:"
                    putStrLn $ name ++ " -- Enter the REPL"
                    putStrLn $ name ++ " [process] -- Run single process"
        
readProgram :: String ->  ThrowsError PiProcess
readProgram input = case parse parseProcess "pi-calculus" input of
                        Left  err -> throwError $ Parser err
                        Right val -> return val 

evalCond :: Env -> Condition -> IOThrowsError Bool
evalCond env (t1 `Equals` t2) = liftM2 (==) (evalTerm env t1) (evalTerm env t2)

evalTerm :: Env -> Term -> IOThrowsError Term
evalTerm env (TVar name) = do
            var <- getVar env name
            case var of
                Term term -> return term
                _         -> throwE $ NotTerm name var  
evalTerm _ (TNum num) = return $ TNum num
evalTerm _ (TStr str) = return $ TStr str
evalTerm env (TFun name args _) = do
            fun <- getVar env name
            argVals <- mapM (evalTerm env) args
            apply fun argVals

apply :: Value -> [Term] -> IOThrowsError Term 
apply (Func fun) args = liftThrows $ fun args
apply e args          = throwE $ NotFunction  ("Found " ++ show e) $ show args

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwE err
liftThrows (Right val) = return val

extractValue :: ThrowsError a -> a 
extractValue (Right v) = v
extractValue (Left  e) = error $ show e

eval :: Env -> PiProcess -> IOThrowsError () 
eval _ Null        = liftIO $ do
                        threadId <- myThreadId
                        putStrLn $ "Stopping Process : " ++ show threadId
                        --myThreadId >>= killThread
eval env (In a (TVar name))  = do
                    chan <- evalChan env a
                    received <- receiveIn chan
                    _ <- defineVar env name received 
                    liftIO $ putStrLn $ "Receiving " ++ show received ++ " On " ++ show a
eval env (Out a b) = do 
                    chan <- evalChan env a
                    bVal <- evalTerm env b
                    sendOut chan (Term bVal)
                    liftIO $ putStrLn $ "Sending " ++ show bVal ++ " On " ++ show a
eval env (Replicate proc) = liftIO (threadDelay 1000000) >> eval env (proc `Conc` Replicate proc)
eval env (p1 `Conc` p2)   = do
                    _ <- liftIO $ forkIO $ do {_ <- runExceptT $ eval env p1; return ()} -- there must be a better way
                    eval env p2
eval env (p1 `Seq` p2)    = do
                    eval env p1
                    eval env p2
eval env (New var@(TVar name))       = do
            _ <- defineVar env name $ Term var
            return ()
eval env (If b p1 p2)     = do
            cond <- evalCond env b
            eval env (if cond then p1 else p2)
eval env (Let (TVar name) t2 p) = do
            term <- evalTerm env t2 
            _ <- defineVar env name $ Term term
            eval env p
eval _ _ = throwE $ Default "undefined action"

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readProgram expr) >>= eval env

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue $ runExceptT (trapError action)

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchE action (return . show)

readTerm :: String -> ThrowsError Term 
readTerm str = case parse parseTerm "Term" str of
                Left  err -> throwError $ Parser err
                Right val -> return val 

evalAndPrint :: Env -> String -> IO ()
evalAndPrint _   []   = return () 
evalAndPrint env expr = evalString env expr >>= putStrLn

runProcess :: String -> IO ()
runProcess expr = coreBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = coreBindings >>= until_ quit (readPrompt "phi >> ") . evalAndPrint
        where
            quit = flip any [":quit",":q"] . (==)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pre prompt action = do
    result <- prompt
    unless (pre result) $ action result >> until_ pre prompt action

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

sendOut :: Channel -> Value -> IOThrowsError () 
sendOut chan val = liftIO $ hPutStrLn (handle chan) $ serialize chan val 

receiveIn :: Channel -> IOThrowsError Value
receiveIn chan = do 
            message <- liftIO $ hGetLine $ handle chan 
            deserialize chan message

evalChan :: Env -> Term -> IOThrowsError Channel
evalChan env (TVar name) = do
            val <- getVar env name
            case val of
                Chan c -> return c
                _      -> throwE $ NotChannel name
evalChan _   (TFun "file" [TStr str] 1) = fileChan str
evalChan _   (TFun "http" [_] 1) = throwE $ Default "http channels undefined"
evalChan env (TNum num) = evalChan env $ TVar . show $ num
evalChan _   (TStr str) = throwE $ NotChannel str
evalChan _ _ = throwE $ Default "undefined channel"
