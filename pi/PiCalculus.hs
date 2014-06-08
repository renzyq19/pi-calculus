module Main where

import Control.Arrow (second)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Monad (liftM, liftM2, unless)
import Control.Monad.Error (throwError)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (catchE, runExceptT, throwE)
import Data.IORef (newIORef, readIORef,writeIORef)
import Data.Maybe (isJust)
import Network.HTTP.Base (Request(..), RequestMethod(..), mkRequest)
import Network.URI (parseURI)
import System.Environment (getArgs, getProgName)
import System.IO (hFlush, stderr, stdin, stdout)

import qualified Data.Map as Map

import Channel
import Parser
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
                 , ("0"      , stdChan stdin) 
                 , ("stdout" , stdChan stdout)
                 , ("1"      , stdChan stdout)
                 , ("stderr" , stdChan stderr)
                 , ("2"      , stdChan stderr)
                 ]

primitives :: [(String      , TermFun)]
primitives = [ ("fst"       , first)
             , ("snd"       , secnd)
             , ("hash"      , unaryId "hash")
             , ("pk"        , unaryId "pk")
             , ("httpRequest", http)
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

true :: Term
true = TBool True

false :: Term
false = TBool False

constId :: String -> TermFun
constId name [] = return $ TFun name [] 0
constId name e  = throwError $ NumArgs name 0 (map Term e)

unaryId :: String -> TermFun
unaryId name [x] =  return $ TFun name [x] 1
unaryId name e  = throwError $ NumArgs name 1 (map Term e)

binaryId :: String ->  TermFun
binaryId name [x,y] = return $ TFun name [x,y] 2 
binaryId name e  = throwError $ NumArgs name 2 (map Term e)

getmsg :: TermFun
getmsg [TFun "sign" [_,y] 2] = return y
getmsg e = throwError $ TypeMismatch "sign" $ map Term e

first :: TermFun
first [TPair p] = return $ fst p
first e = throwError $ TypeMismatch "pair" $ map Term e 

secnd :: TermFun
secnd [TPair p] = return $ snd p
secnd e = throwError $ TypeMismatch "pair" $ map Term e 

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
checksign [TFun "pk" [k1] 1 , TFun "sign" [k2,_] 2 ] = return $ TBool (k1 == k2)
checksign e = throwError $ TypeMismatch "(pk(var),sign(var,var))" $ map Term e

http :: TermFun
http [TStr url] = case httpGetRequest http' of
                        Just x  -> return $ TStr x
                        Nothing -> throwError $ Default "malformed uri"
    where
        http'  = if take 7 url /= http'' then http'' ++ url else url
        http'' = "http://" 
http _ = throwError $ Default "undefined"

main :: IO ()
main = do
        name <- getProgName
        args <- getArgs
        case args of
            []  -> runRepl coreBindings
            [x] -> readFile x >>= runProcess coreBindings 
            _   -> do
                    putStrLn "Use:"
                    putStrLn $ name ++ " -- Enter the REPL"
                    putStrLn $ name ++ " [process] -- Run single process"
        


load :: String -> IOThrowsError [PiProcess]
load filename = liftIO (readFile filename) >>= liftThrows . readProcesses 
                        

                        
evalCond :: Env -> Condition -> IOThrowsError Bool
evalCond env (t1 `Equals` t2) = liftM2 (==) (evalTerm env t1) (evalTerm env t2)

evalTerm :: Env -> Term -> IOThrowsError Value
evalTerm env (TVar name) = getVar env name
evalTerm _   (TNum num) = return $ Term $ TNum num
evalTerm _   (TStr str) = return $ Term $ TStr str
evalTerm _   (TBool b ) = return $ Term $ TBool b
evalTerm env (TPair (t1,t2)) = do
            a <- evalTerm env t1
            b <- evalTerm env t2
            case (a,b) of 
                (Term c, Term d) -> return $ Term $ TPair (c,d)
                _                -> throwE $ Default "pair not given two terms"
evalTerm env (TFun "anonChan" [] 0) = do
            port <- assignFreePort env
            liftM Chan $ liftIO $ newChan Init ("localhost:" ++ show port) port 
evalTerm _   (TFun "anonChan" [TNum n] 1) = liftM Chan $ liftIO $ newChan Init ("localhost:"++ show n) n 
evalTerm env (TFun "httpChan" [TStr addr] 1) = do
            port <- assignFreePort env
            liftM Chan $ liftIO $ newChan Connect (addr ++ ":80") port
evalTerm env (TFun "chan" [TStr addr] 1) = do
            port <- assignFreePort env
            liftM Chan $ liftIO $ newChan Connect addr port
evalTerm env (TFun name args _) = do
            fun <- getVar env name
            argVals <- mapM (evalTerm env) args
            apply fun argVals


assignFreePort :: Env -> IOThrowsError Integer
assignFreePort env = do
            Term (TNum port) <- getVar env counterRef
            _ <- setVar env counterRef $ Term $ TNum $ port + 1
            if port == 2 ^ (16 :: Integer)
                then error "HOW MANY CHANNELS DO YOU WANT?!" 
                else return port


apply :: Value -> [Value] -> IOThrowsError Value 
apply (PrimitiveFunc fun) args = do
                        ts <- extracTerms args
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

extracTerms :: [Value] -> IOThrowsError [Term]
extracTerms ts
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
eval env (In a (TVar name)) = do
                chan <- evalChan env a
                received <- receiveIn chan
                _ <- defineVar env name received 
                return ()
eval env (Out a b) = do 
                chan <- evalChan env a
                bVal <- evalTerm env b
                sendOut chan bVal
                return ()
eval env (Replicate proc) = liftIO (threadDelay 1000000) >> eval env (Conc [proc, Replicate proc])
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
eval env (New var@(TVar name)) = do
                _ <- defineVar env name $ Term var
                return ()
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
eval env (Let (TFun name args _) t2 (Just p)) = 
            defineLocalFun env name args t2 p
eval env (Let (TFun name args _) t2 Nothing)  = 
            defineGlobalFun env name args t2
eval env (Atom (TFun "load" [TStr file] 1)) = do
            procs <- load file  
            eval env $ foldl Seq Null procs
eval env (Atom (TFun "env" [] 0)) = do
            e <- liftIO $ readIORef env
            liftIO $ mapM_ (\(k,v) -> putStrLn $ k ++ ": " ++ show v) $ Map.toAscList e
eval env (Atom p)  = do
            proc <- evalProcess env p
            eval env proc
eval _ _ = throwE $ Default "undefined action"

defineGlobalFun :: Env -> String -> [Term] -> Value -> IOThrowsError ()
defineGlobalFun env name args term = do
            _ <- defineVar env name $ makeFun args term env
            return ()

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
sendOut _  (Chan (Channel _ _ False _)) = throwE $ Default "Channel not serialisable" 
sendOut chan val = liftIO $ send chan $ show val

receiveIn :: Channel -> IOThrowsError Value
receiveIn chan = do
        msg  <- liftIO $ receive chan
        term <- liftThrows $ readTerm msg
        case term of
            TFun "<chan>" ex _ -> decodeChannel ex
            _ -> return $ Term term
            where
            decodeChannel e = do
                let extraStrings = map (\(TStr x) -> x) e
                let extraData = map (second tail . break (==dataBreak)) extraStrings
                case getChannelData extraData of
                    Just (h,p)  -> liftM Chan $ liftIO $ newChan Connect h p
                    Nothing -> throwE $ Default "incomplete data in channel"

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
                
httpGetRequest :: String -> Maybe String
httpGetRequest str = do
        uri <- parseURI str
        return $ show (mkRequest GET uri :: Request String)
