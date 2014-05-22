module Channel  (
    Channel (..),
    stdStrChan  ,
    newChan     ,
    dataBreak   )
    where

import qualified Network as N
import Network.BSD (getHostName)
import System.IO (Handle, hGetContents, hGetLine, hPrint, hPutStrLn, hShow)
import System.IO.Error (catchIOError)
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.MVar

import Types 

data Channel = Channel {
               chanType    :: ChannelType
             , clientPort  :: Integer
             , send        :: String -> IO ()
             , receive     :: IO String
             , extra       :: [String]
             }

stdStrChan :: Handle -> Channel
stdStrChan h = Channel String (-1) write rd []
    where
        write = hPutStrLn h
        rd = hGetLine h

newChan :: ChannelType -> String -> Integer -> IO Channel
newChan t host cp = do {
            currentHost <- getHostName ;
            case hostName of 
            "localhost" 
                    | t == Internal   -> newInternalChan currentHost hostPort cp
                    | cp == -1        -> newForeignChan t hostName hostPort 
                    | otherwise       -> newLocalChan t cp
            _           -> newForeignChan t hostName hostPort } 
               where
               (hostName, _:hostPort) = break (==':') host

newInternalChan :: String -> String -> Integer -> IO Channel
newInternalChan hostName hostPort cp = return $ Channel Internal cp s r ex
    where
       r   = N.withSocketsDo $ do
            inSock <- N.listenOn $ N.PortNumber $ fromIntegral cp
            (inHandle,_,_) <- N.accept inSock
            msg <- hGetLine inHandle
            N.sClose inSock
            return msg
       s v = N.withSocketsDo $ do
            _ <- forkIO $ do
                outHandle <- waitForConnect hostName $ N.PortNumber $ port hostPort
                hPutStrLn outHandle v
            return ()
       ex = zipWith (\a b -> (a ++ dataBreak : b))  ["host","clientPort","type"] [hostName ++ ":" ++ hostPort,show cp,show Internal]

newLocalChan :: ChannelType -> Integer -> IO Channel
newLocalChan t cp = N.withSocketsDo $ do
    hanVar <- newEmptyMVar
    _ <- forkIO $ do
        inSock <- N.listenOn $ N.PortNumber $ fromIntegral cp
        (inHandle,_,_)  <- N.accept inSock
        putMVar hanVar inHandle
    currentHost <- getHostName
    let ex' = ex ++ ["host" ++ dataBreak :currentHost]
    return $ Channel t cp (send' hanVar) (receive' hanVar) ex'
      where
       ex = zipWith (\a b -> (a ++ dataBreak : b))  ["clientPort","type"] [show cp,show t]

newForeignChan :: ChannelType -> String -> String -> IO Channel
newForeignChan t hostName hostPort = N.withSocketsDo $ do
    hanVar <- newEmptyMVar
    _ <- forkIO $ do
        outHandle <- waitForConnect hostName $ N.PortNumber $ port hostPort
        putMVar hanVar outHandle
    return $ Channel t 0 (send' hanVar) (receive' hanVar) ex
    where
       ex = zipWith (\a b -> a ++ dataBreak : b)  ["host","clientPort","type"] [hostName ++ ":" ++ hostPort,"-1",show t]

port :: String -> N.PortNumber
port s = fromIntegral  (read s :: Integer)

dataBreak :: Char
dataBreak = '#'

waitForConnect :: N.HostName -> N.PortID -> IO Handle
waitForConnect h p = N.connectTo h p `catchIOError`
                                    (\_ -> do
                                        threadDelay 10000
                                        putStrLn "waiting for connection"
                                        waitForConnect h p)

send' :: MVar Handle -> String -> IO ()
send' hanVar v = do
        han <- takeMVar hanVar
        hPutStrLn han v
        putMVar hanVar han

receive' :: MVar Handle -> IO String
receive' hanVar = do
        han <- readMVar hanVar
        hGetLine han

printH :: Handle -> IO ()
printH h = do
   hstr <- hShow h
   putStrLn hstr
