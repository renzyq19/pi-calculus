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
setVar _ "_" _ = return $ Term $ TVar "_" -- to allow wildcard matching
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
        name <- getProgName
        args <- getArgs
        case args of
            []  -> runRepl coreBindings
            [x] -> readFile x >>= runProcess coreBindings 
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
evalTerm env (TVar name) = getVar env name
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
            str <- evalTerm env t
            case str of 
                Term (TStr s) -> return s
                _             -> throwE $ Default $ "Not a string : " ++ show t

evalToInt :: Env -> Term -> IOThrowsError Integer
evalToInt env t = do
            num <- evalTerm env t
            case num of 
                Term (TNum n) -> return n
                _             -> throwE $ Default $ "Not a number : " ++ show t

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
extractTerms ts
        | all isTerm ts = return $ map (\(Term t) -> t) ts
        | otherwise     = throwE $ Default "not all terms"
        
isTerm :: Value -> Bool
isTerm (Term _) = True
isTerm _ = False
        
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = either throwE return 

extractValue :: ThrowsError a -> a 
extractValue (Right v) = v
extractValue (Left  e) = error $ show e

eval :: Env -> PiProcess -> IOThrowsError () 
eval _ Null = return ()
eval env (In a b) = do
                chan <- evalChan env a
                receiveAndMatch env b chan
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
eval env (New var@(TVar name)) = void $ defineVar env name $ Term var
eval env (If b p1 p2) = do
                cond <- evalCond env b
                eval env (if cond then p1 else p2)
eval env (Let (TVar name) (Term t2) (Just p)) = do
                val <- evalTerm env t2 
                newEnv <- liftIO $ bindVars env [(name,val)]
                eval newEnv p
eval env (Let (TVar name) (Term t2) Nothing) = do
                val <- evalTerm env t2
                _ <- defineVar env name val
                return ()
eval env (Let (TVar name) proc@(Proc _) (Just p)) = do
                newEnv <- liftIO $ bindVars env [(name,proc)]
                eval newEnv p
eval env (Let (TVar name) proc@(Proc _) Nothing) = do
                _ <- defineVar env name proc
                return ()
eval env (Let (TFun name args) t2 (Just p)) = 
            defineLocalFun env name args t2 p
eval env (Let (TFun name args) t2 Nothing)  = 
            defineGlobalFun env name args t2
eval env (Atom (TFun "load" [TStr file])) = do
            procs <- load file  
            eval env $ foldl Seq Null procs
eval env (Atom (TVar "env")) = do
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

matchVar :: String -> String -> IOThrowsError [(String,Value)]
matchVar name msg = do
        term <- liftThrows $ readTerm msg
        v <- case term of
                TFun "<chan>" ex -> decodeChannel ex
                _ -> return $ Term term
        return [(name, v)]
                where
                decodeChannel e = do
                    let extraStrings = map (\(TStr x) -> x) e
                    let extraData = map (second tail . break (==dataBreak)) extraStrings
                    case getChannelData extraData of
                        Just (h,p)  -> liftM Chan $ liftIO $ newChan Connect h p
                        Nothing -> throwE $ Default "incomplete data in channel"

receiveAndMatch :: Env -> Term -> Channel -> IOThrowsError ()
receiveAndMatch env term chan = do
        str <- liftIO $ receive chan
        let ls = lines str
        bindings <- case term of
            TFun "httpReq"  args -> matchRequestData args ls
            TFun "httpResp" args -> matchResponseData args ls 
            TVar v               -> matchVar v str
            _                    -> throwE $ Default $ "Can't handle " ++ show term ++ " yet"
        newE <- liftIO $ bindVars env bindings
        e <- liftIO $ readIORef newE
        liftIO $ writeIORef env e
        return ()
        
matchRequestData :: [Term] -> [String] -> IOThrowsError [(String,Value)]
matchRequestData [TVar uri, headerss , TVar method] ls = do  
    (m, r, hs) <- case parseRequestHead ls of
        Left _         -> throwE $ Default ""
        Right (m,r,h) -> return (m,r,h)
    case headerss of
        TVar h -> return $ zip [uri,h,method] $ map Term [TStr $ show r, TList $ map (TStr . show) hs , TStr $ show m] 
        _      -> throwE $ Default "I CAN'T HANDLE THIS SHIT"
matchRequestData [TVar d] ls = do
    (m, r, hs) <- case parseRequestHead ls of
        Left _     -> throwE $ Default ""
        Right (m,r,h) -> return (m,r,h)
    return [(d , Term $ TData $ Req $ Request r m hs "")]
matchRequestData _ _ = todo
    
matchResponseData :: [Term] -> [String] -> IOThrowsError [(String,Value)]
matchResponseData [TVar code, TVar reason, headerss, TVar bdy] ls = do  
    (c, r, hs) <- case parseResponseHead ls of
        Left _     -> throwE $ Default "Failed to parse httpData"
        Right (c,r,h) -> return (c,r,h)
    case headerss of
        TVar h -> return $ zip [code,reason,h,bdy] $ map Term [TStr $ show c, TStr r,  TList $ map (TStr . show) hs,TStr $ msgBody ls] 
        _      -> throwE $ Default "I CAN'T HANDLE THIS SHIT"
matchResponseData [TVar d] ls = do
    (c, r, hs) <- case parseResponseHead ls of
        Left _     -> throwE $ Default ""
        Right (c,r,h) -> return (c,r,h)
    return [(d , Term $ TData $ Resp $ Response c r hs (msgBody ls))]
matchResponseData _ _ = todo


msgBody :: [String] -> String
msgBody = unlines . dropWhile (/= crlf)
    where
        crlf = "\r"

todo :: IOThrowsError a
todo = throwE $ Default "TODO"

getChannelData :: [(String,String)] -> Maybe (String, Integer)
getChannelData ex = do
        host         <- lookup "host" ex
        cp           <- lookup "clientPort" ex
        return (host,read cp)

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
