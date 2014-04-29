{-# LANGUAGE TupleSections #-}
module Main where

import Control.Arrow (second)
import Control.Monad (liftM, liftM2)
import Control.Monad.Error (Error(..), ErrorT(..), MonadError, catchError, throwError )
import Control.Monad.Trans (liftIO)
import Data.IORef (IORef, newIORef, readIORef,writeIORef)
import Data.List (intercalate)
import Data.Maybe (isJust)
import System.Environment (getArgs)
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

data Term = TVar Variable Type
          | TFun Name [Term] Int Type
            deriving (Eq)

data Value = Process PiProcess 
           | Term Term
           | Function TermFun

data PiError = NumArgs Name Integer [Term]
             | TypeMismatch String Value
             | Parser ParseError
             | NotFunction String String
             | UnboundVar String String
             | Default String

type IOThrowsError = ErrorT PiError IO 
type ThrowsError   = Either PiError

type Variable  = String
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
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> Value -> IOThrowsError Value
setVar envRef var val = do env <- liftIO $ readIORef envRef
                           maybe (throwError $ UnboundVar "Setting an unbound variable" var)
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
showValue (Process p)  = show p
showValue (Term t)     = show t
showValue (Function _) = "Function" 

showPi :: PiProcess -> String
showPi Null = "0"
showPi (In c m) =  "in(" ++ show c ++ "," ++ show m ++ ")"
showPi (Out c m) =  "out(" ++ show c ++ "," ++  show m ++ ")"
showPi (Replicate proc) =  "!(" ++ show proc ++ ")"
showPi (p1 `Conc` p2) = show p1 ++ "|\n" ++ show p2
showPi (p1 `Seq` Null) = show p1
showPi (p1 `Seq` p2) = show p1 ++ ";\n" ++ show p2 
showPi (New n)   = "new " ++ show n
showPi (If c p1 p2) = "if " ++ show c ++ " then " ++ show p1 ++ " else " ++ show p2
showPi (Let n t p) = "let " ++ show n ++ " = " ++ show t ++ " in\n" ++ show p

showTerm :: Term -> String
showTerm (TVar x _) = x
showTerm (TFun n [] 0 _) = n ++ "()"
showTerm (TFun n ts _ _) = n ++ "(" ++ intercalate "," (map show ts) ++ ")"

showCond :: Condition -> String
showCond (t1 `Equals` t2) = show t1 ++ " == " ++ show t2

showError :: PiError -> String
showError (UnboundVar message var)      = message ++ ": " ++ var
showError (NotFunction message fun)     = message ++ ": " ++ fun
showError (NumArgs name expected found) = "Expected " ++ show name ++ show expected ++ " args; found values "
                                          ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found "
                                          ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr


instance Show PiProcess where show = showPi
instance Show Term where show = showTerm
instance Show Condition where show = showCond
instance Show Value where show = showValue
instance Show PiError where show = showError

instance Error PiError where
    noMsg = Default "an error has occured" 
    strMsg= Default

unwordsList :: [Term] -> String
unwordsList = unwords . map show

parseNull :: Parser PiProcess
parseNull = do
            paddedChar '0'
            return Null

parseIn :: Parser PiProcess
parseIn = do
            string "in("
            name <- parseTerm
            paddedComma
            var  <- parseTerm
            char ')'
            parseSeq $ In name var 

parseOut :: Parser PiProcess
parseOut = do
            string "out("
            name <- parseTerm
            paddedComma
            term  <- parseTerm
            char ')'
            parseSeq $ Out name term 

parseReplicate :: Parser PiProcess
parseReplicate = do
            string "!("
            process <- parseProcess
            char ')'
            return $ Replicate process

paddedChar :: Char ->  Parser ()
paddedChar ch = do
            spaces
            char ch
            spaces

parseSeq :: PiProcess -> Parser PiProcess
parseSeq p1 = do
            paddedChar ';'
            p2 <- parseProcess
            return $ p1 `Seq` p2

parseNew :: Parser PiProcess
parseNew = do
            string "new"
            spaces
            name <- parseTerm
            parseSeq $ New name

parseIf :: Parser PiProcess
parseIf = do
            string "if" 
            spaces
            cond <- parseCondition
            spaces
            string "then"
            spaces
            p1 <- parseProcess
            spaces
            string "else"
            spaces
            p2 <- parseProcess
            return $ If cond p1 p2

parseLet :: Parser PiProcess
parseLet = do
            string "let"
            spaces
            name <- parseTerm
            paddedChar '='
            term <- parseTerm
            spaces
            string "in"
            spaces
            p   <- parseProcess
            return $ Let name term p

parseCondition :: Parser Condition
parseCondition = do
            t1 <- parseTerm
            spaces
            string "=="
            spaces
            t2 <- parseTerm
            return $ t1 `Equals` t2

parseTVar :: Parser Term
parseTVar = liftM (flip TVar "dummy") readVar 

parseTFun :: Parser Term
parseTFun = do
            name <- readVar
            spaces
            args <- bracketed $ sepBy parseTerm paddedComma
            return $ TFun name args (length args) "dummy"

readVar :: Parser Name
readVar = do
            frst <- letter
            rest <- many $ letter <|> digit <|> symbol
            return $ frst:rest

symbol :: Parser Char
symbol = oneOf ".'"

paddedComma :: Parser ()
paddedComma = paddedChar ','


parseTerm :: Parser Term
parseTerm =  try parseTFun
         <|> parseTVar

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
                    char '('
                    spaces
                    res <- parser
                    spaces
                    char ')'
                    return res

type TermFun = [Term] -> ThrowsError Term


bindVars :: Env -> [(String , Value)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where
        extendEnv bs env     = liftM (++ env) (mapM addBinding bs)
        addBinding (var,val) = liftM (var,) $ newIORef val

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map  (second Function) primitives)

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
constId name [] = return $ TFun name [] 0 "dummy"
constId name e@_  = throwError $ NumArgs name 0 e

unaryId :: String -> TermFun
unaryId name [x] =  return $ TFun name [x] 1 "dummy"
unaryId name e@_  = throwError $ NumArgs name 1 e

binaryId :: String ->  TermFun
binaryId name [x,y] = return $ TFun name [x,y] 2 "dummy"
binaryId name e@_  = throwError $ NumArgs name 2 e

getmsg :: TermFun
getmsg [TFun "sign" [_,y] 2 _] = return y
getmsg l@_ = error $  "getmsg expected sign(x,y) got: " ++ show l

first :: TermFun
first [TFun "pair" [x, _] 2 _] = return x
first _  = error "fst not given pair"

secnd :: TermFun
secnd [TFun "pair" [_,y] 2 _] = return y
secnd _ = error "second not given pair"

http :: TermFun
http [TVar _ _] = undefined

sdec :: TermFun
sdec [k1, TFun "senc" [k2,y] 2 _]
    |k1 == k2  = return y
    |otherwise = error "keys not the same in sdec"
sdec _ = error "sdec not given pair"

adec :: TermFun
adec [x , TFun "aenc" [TFun "pk" [k] 1 _, y ] 2 _]
    | x == k = return y
    | otherwise= error  "keys not same in adec" 
adec e@_ = error $ "checksign expected (x,aenc(pk(x),y)), got: " ++ show e

checksign :: TermFun
checksign [TFun "pk" [k1] 1 _, TFun "sign" [k2,_] 2 _]
    | k1 == k2 = constId "true" [] 
    | otherwise= constId "false" []
checksign e@_ = error $ "checksign expected (pk(x),sign(x,y)), got: " ++ show e

main :: IO ()
main = do
        args <- getArgs 
        let f = case args of
                    []  -> readFile "test.pi" 
                    [x] -> return x
        progs <- liftM lines f
        putStrLn $ intercalate  "\n\n" $ map readProgram progs
        putStrLn ""

readProgram :: String ->  String
readProgram input = case parse parseProcess "pi-calculus" input of
                        Left  err -> show err
                        Right val -> show val 

evalCond :: Env -> Condition -> IOThrowsError Bool
evalCond env (t1 `Equals` t2) = liftM2 (==) (evalTerm env t1) (evalTerm env t2)

evalTerm :: Env -> Term -> IOThrowsError Term
evalTerm _ val@(TVar _ _) = return val
evalTerm env (TFun name args _ _) = do
            fun <- getVar env name
            argVals <- mapM (evalTerm env) args
            apply fun argVals

apply :: Value -> [Term] -> IOThrowsError Term 
apply (Function fun) args = liftThrows $ fun args
apply e args            = throwError $ NotFunction  ("Found " ++ show e) $ show args

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue $ runErrorT (trapError action)

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right v) = v

eval :: Env -> PiProcess -> IO ()
eval _ Null               = putStrLn "Stopping Process..."
eval _ (In _ _)           = undefined
eval _ (Out _ _)          = undefined
eval env (Replicate proc) = eval env $ proc `Conc` Replicate proc
eval _ (_ `Conc` _)       = undefined
eval _ (_ `Seq` _)        = undefined 
eval _ (New _)            = undefined 
eval _ (If{})             = undefined 
eval _ (Let{})            = undefined

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readTerm expr) >>= evalTerm env


readTerm :: String -> ThrowsError Term 
readTerm str = case parse parseTerm "(test)" str of
                Left  err -> throwError $ Parser err
                Right val -> return val 

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runTerm :: String -> IO ()
runTerm expr = primitiveBindings >>= flip evalAndPrint expr
