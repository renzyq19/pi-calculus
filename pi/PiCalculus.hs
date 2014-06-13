module Main where

import Control.Arrow (second)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Monad (liftM, liftM2, unless, void)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (catchE, runExceptT, throwE)
import Data.IORef (newIORef, readIORef,writeIORef)
import Data.Maybe (isJust)
import Network.HTTP.Base (Request(..),Response(..), parseResponseHead, parseRequestHead)
import System.Environment (getArgs, getProgName)
import System.IO (hFlush, stderr, stdin, stdout)
import System.IO.Error (tryIOError)

import qualified Data.Map as Map

import Channel
import Parser
import Paths_pi_calculus
import PatternMatching
import Primitives
import TypDefs

nullEnv :: IO Env
nullEnv = newIORef Map.empty

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . Map.lookup var) $ readIORef envRef

getVar :: Env -> String -> IOThrowsError Value 
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwE $ UnboundVar "Getting an unbound variable" var)
                             return
                             (Map.lookup var env)

setVar :: Env -> String -> Value -> IOThrowsError Value
setVar _ "_" _ = return $ Term $ TVar "_" Nothing -- to allow wildcard matching
setVar envRef var val = do env <- liftIO $ readIORef envRef
                           maybe (throwE $ UnboundVar "Setting an unbound variable" var)
                                 (return $ liftIO $ writeIORef envRef $ Map.insert var val env)
                                 (Map.lookup var env)
                           return val
                           
defineVar :: Env -> String -> Value -> IOThrowsError Value
defineVar envRef var val = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var val >> return val
        else liftIO $ do
            env      <- readIORef envRef
            writeIORef envRef $ Map.insert var val env
            return val

bindVars :: Env -> [(String , Value)] -> IO Env
bindVars envRef bindings = do
                env <- readIORef envRef
                newIORef $ Map.union (Map.fromList bindings) env

coreBindings :: IO Env
coreBindings = do
                n <- nullEnv 
                e1 <- bindVars n (map (second PrimitiveFunc) primitives) 
                e2 <- bindVars e1 (map (second Chan) nativeChannels)
                net <- newDummyChan
                bindVars e2 [(counterRef, Term $ TNum lowestPort),
                             ("localnet"     , Chan net)]
                where 
                    lowestPort = 2^(15::Integer) + 2^(14::Integer)

counterRef :: String
counterRef = "###"

nativeChannels :: [(String   , Channel)]
nativeChannels = [ ("stdin"  , stdChan stdin) 
                 , ("stdout" , stdChan stdout)
                 , ("stderr" , stdChan stderr)
                 ]


main :: IO ()
main = do
        name   <- getProgName
        args   <- getArgs
        pilude <- getDataFileName "pilude.pi"
        case args of
            []  -> runRepl coreBindings
            [x] -> liftM (("&load("++pilude++");")++) (readFile x) >>= runProcess coreBindings 
            _   -> do
                    putStrLn           "Use:"
                    putStrLn $ name ++ " -- Enter the REPL"
                    putStrLn $ name ++ " [process] -- Run single process"

load :: String -> IOThrowsError [PiProcess]
load filename = do
                f <- liftIO $ tryIOError (readFile filename)
                case f of
                    Left _   -> throwE $ Default "File does not exist" 
                    Right f' -> liftThrows . readProcesses $ f'
                        
evalCond :: Env -> Condition -> IOThrowsError Bool
evalCond env (t1 `Equals` t2) = liftM2 (==) (evalTerm env t1) (evalTerm env t2)

evalTerm :: Env -> Term -> IOThrowsError Value
evalTerm env (TVar name _) = getVar env name
evalTerm _   (TNum num) = return $ Term $ TNum num
evalTerm _   (TStr str) = return $ Term $ TStr str
evalTerm _   (TBool b ) = return $ Term $ TBool b
evalTerm _   (TData d ) = return $ Term $ TData d
evalTerm env   (TList ls) = do
    vs <- mapM (evalTerm env) ls
    ts <- extractTerms vs
    return $ Term $ TList ts
evalTerm env (TPair (t1,t2)) = do
            a <- evalTerm env t1
            b <- evalTerm env t2
            case (a,b) of 
                (Term c, Term d) -> return $ Term $ TPair (c,d)
                _                -> throwE $ Default "pair not given two terms"
evalTerm env (TFun "anonChan" []) = do
            port <- assignFreePort env
            liftM Chan $ liftIO $ newChan Init "localhost" port 
evalTerm env (TFun "anonChan" [n]) = do
            port <- evalToInt env n
            c <- liftIO $ newChan Init "localhost" port 
            return $ Chan c
evalTerm env (TFun "httpChan" [a]) = do
            host <- evalToString env a
            liftM Chan $ liftIO $ newChan Connect host 80
evalTerm env (TFun "chan" [a,n]) = do
            host <- evalToString env a
            port <- evalToInt env n
            liftM Chan $ liftIO $ newChan Connect host port
evalTerm env (TFun name args) = do
            fun <- getVar env name
            argVals <- mapM (evalTerm env) args
            apply fun argVals

evalToString :: Env -> Term -> IOThrowsError String
evalToString env t = do
            s <- evalTerm env t
            str <- extractTerm s
            extractString str

extractString :: Term -> IOThrowsError String
extractString str =
            case str of 
                TStr s -> return s
                _      -> throwE $ Default $ "Not a string : " ++ show str 

evalToInt :: Env -> Term -> IOThrowsError Integer
evalToInt env t = do
            n <- evalTerm env t
            num <- extractTerm n
            extractInt num

extractInt :: Term -> IOThrowsError Integer
extractInt num = 
            case num of 
                TNum n -> return n
                _      -> throwE $ Default $ "Not a number : " ++ show num

assignFreePort :: Env -> IOThrowsError Integer
assignFreePort env = do
            Term (TNum port) <- getVar env counterRef
            _ <- setVar env counterRef $ Term $ TNum $ port + 1
            if port == 2 ^ (16 :: Integer)
                then error "HOW MANY CHANNELS DO YOU WANT?!" 
                else return port

apply :: Value -> [Value] -> IOThrowsError Value 
apply (PrimitiveFunc fun) args = do
                        ts <- extractTerms args
                        res <- liftThrows $ fun ts
                        return $ Term res
apply (Func parms bdy closre) args =
    if num parms /= num args 
        then throwE $ NumArgs "user-defined" (num parms) args
        else do
             clos <- liftIO (bindVars closre $ zip parms args)
             case bdy of
                Term t -> evalTerm clos t
                Proc p -> eval clos p >> return bdy
                _      -> throwE $ Default "this function makes no sense"
    where
        num = toInteger . length
apply e _ = throwE $ NotFunction "expecting a function found" $ show e

extractTerms :: [Value] -> IOThrowsError [Term]
extractTerms = mapM extractTerm

extractTerm :: Value -> IOThrowsError Term
extractTerm (Term t) = return t
extractTerm e        = throwE $ Default $ "Trying to extract term from: " ++ show e
        
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = either throwE return 

extractValue :: ThrowsError a -> a 
extractValue (Right v) = v
extractValue (Left  e) = error $ show e

eval :: Env -> PiProcess -> IOThrowsError () 
eval _ Null = return ()
eval env (In a v@(TVar b t)) = do
                chan <- evalChan env a
                term <- receiveIn chan t
                bindings <- case term of
                    TFun "<chan>" ex -> do
                            ch <- decodeChannel ex
                            return [(b,ch)]
                    _ -> liftThrows $ match v term
                mapM_ (uncurry (defineVar env)) bindings
                return ()
                    where
                    decodeChannel e = do
                        extraStrings <-  mapM extractString e
                        case getChannelData extraStrings of
                            Just (h,p)  -> liftM Chan $ liftIO $ newChan Connect h p
                            Nothing -> throwE $ Default "incomplete data in channel"
eval env (Out a b) = do 
                chan <- evalChan env a
                bVal <- evalTerm env b
                sendOut chan bVal
                return ()
eval env (Replicate proc) = liftIO (threadDelay 100000) >> eval env (Conc [proc, Replicate proc])
eval env (Conc [])     = eval env Null
eval env (Conc procs)  = do
                var <- liftIO newEmptyMVar 
                mapM_ (forkProcess var) procs
                res <- liftIO $ takeMVar var
                case res of
                    Left err -> throwE err
                    Right _  -> return ()
        where
            forkProcess var proc = liftIO $ forkIO $ do
                        res <- runExceptT $ eval env proc
                        _ <- tryPutMVar var res
                        return ()
eval env (p1 `Seq` p2) = do
                eval env p1
                eval env p2
eval env (New var@(TVar name _)) = void $ defineVar env name $ Term var
eval env (If b p1 p2) = do
                cond <- evalCond env b
                eval env (if cond then p1 else p2)
eval env (Let (TVar name _) (Term t2) (Just p)) = do
                val <- evalTerm env t2 
                newEnv <- liftIO $ bindVars env [(name,val)]
                eval newEnv p
eval env (Let (TVar name _) (Term t2) Nothing) = do
                val <- evalTerm env t2
                _ <- defineVar env name val
                return ()
eval env (Let (TVar name _) proc@(Proc _) (Just p)) = do
                newEnv <- liftIO $ bindVars env [(name,proc)]
                eval newEnv p
eval env (Let (TVar name _) proc@(Proc _) Nothing) = do
                _ <- defineVar env name proc
                return ()
eval env (Let (TFun name args) t2 (Just p)) = 
            defineLocalFun env name args t2 p
eval env (Let (TFun name args) t2 Nothing)  = 
            defineGlobalFun env name args t2
eval env (Let t1 (Term t2) (Just p)) = do
                val <- evalTerm env t2 
                case val of 
                    Term term -> do
                        bindings <- liftThrows $ match t1 term
                        newEnv <- liftIO $ bindVars env bindings
                        eval newEnv p
                    _         -> throwE $ Default "Can only pattern match against Terms"
eval env (Let t1 (Term t2) Nothing) = do 
                val <- evalTerm env t2 
                case val of 
                    Term term -> do
                        bindings <- liftThrows $ match t1 term
                        mapM_ (uncurry (defineVar env)) bindings
                    _         -> throwE $ Default "Can only pattern match against Terms"
eval env (Atom (TFun "load" [TStr "pilude.pi"])) = do
            pilude <- liftIO $ getDataFileName "pilude.pi"
            eval env (Atom (TFun "load" [TStr pilude]))
eval env (Atom (TFun "load" [TStr file])) = do
            procs <- load file  
            eval env $ foldl Seq Null procs
eval env (Atom (TVar "env" Nothing)) = do
            e <- liftIO $ readIORef env
            liftIO $ mapM_ (\(k,v) -> putStrLn $ k ++ ": " ++ show v) $ Map.toAscList e
eval env (Atom p@(TFun{})) = void $ evalProcess env p
eval env (Atom p) = do
            proc <- evalProcess env p
            eval env proc
eval _ _ = throwE $ Default "undefined action"

defineGlobalFun :: Env -> String -> [Term] -> Value -> IOThrowsError ()
defineGlobalFun env name args term = void $ defineVar env name $ makeFun args term env

defineLocalFun :: Env -> String -> [Term] -> Value -> PiProcess -> IOThrowsError ()
defineLocalFun env name args term p = do
            clos <- liftIO $ bindVars env [(name, makeFun args term env)]
            eval clos p

makeFun :: [Term] -> Value -> Env -> Value
makeFun args = Func (map show args)

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readProcess expr) >>= eval env

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue $ runExceptT (trapError action)

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchE action (return . show)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
            res <- evalString env expr 
            case res of
                "()"  -> return ()
                _     -> putStrLn res

runProcess :: IO Env -> String -> IO ()
runProcess core expr = core >>= flip evalAndPrint expr

runRepl :: IO Env -> IO ()
runRepl core = core >>= until_ quit (readPrompt "phi>") . evalAndPrint
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
sendOut chan v@(Chan c) = if serialisable c
                        then liftIO $ send chan $ show v
                        else throwE $ Default "Channel not serialisable" 
sendOut chan val = liftIO $ send chan $ show val


receiveIn :: Channel -> Maybe Type -> IOThrowsError Term
receiveIn chan t = do
        str <- liftIO $ receive chan
        case t of
                    Just HttpRequest  -> makeHttpRequest str
                    Just HttpResponse -> makeHttpResponse str
                    _                 -> liftThrows $ readTerm str

makeHttpRequest :: String -> IOThrowsError Term
makeHttpRequest str = do
    let ls = lines str
    (r,u, hs) <- case parseRequestHead ls of
        Left _     -> throwE $ Default "Malformed HTTP Request"
        Right (r,u,h) -> return (r,u,h)
    return $ TData $ Req $ Request u r hs (msgBody ls)

makeHttpResponse :: String -> IOThrowsError Term
makeHttpResponse str = do
    let ls = lines str
    (c, r, hs) <- case parseResponseHead ls of
        Left _     -> throwE $ Default "Malformed HTTP Request"
        Right (c,r,h) -> return (c,r,h)
    return $ TData $ Resp $ Response c r hs (msgBody ls)

msgBody :: [String] -> String
msgBody = unlines . dropWhile (/= crlf)
    where
        crlf = "\r"

todo :: IOThrowsError a
todo = throwE $ Default "TODO"


evalChan :: Env -> Term -> IOThrowsError Channel
evalChan env t = do
            chan <- evalTerm env t
            case chan of
                Chan c -> return c
                _      -> throwE $ NotChannel $ show t

evalProcess :: Env -> Term -> IOThrowsError PiProcess
evalProcess env t = do
            proc <- evalTerm env t
            case proc of
                Proc p -> return p
                _      -> throwE $ NotProcess $ show t
