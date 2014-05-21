module Channel where

import qualified Network as N
import System.IO (Handle, hFlush, hGetContents, hGetLine, hPrint, hPutStrLn, hShow)
import System.IO.Error (catchIOError)
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.MVar

data Channel = Channel {
               chanType    :: String
             , clientPort  :: Integer
             , send        :: String -> IO ()
             , receive     :: IO String
             , extra       :: [String]
             }

stdStrChan :: Handle -> Channel
stdStrChan h = Channel "string" (-1) write rd []
    where
        write = hPutStrLn h
        rd = hGetLine h

newChan :: String -> String -> Integer -> IO Channel
newChan = newInternalChan

newInternalChan :: String -> String -> Integer -> IO Channel
newInternalChan t host cp = return $ Channel t cp ss rr ex
    where
       rr   = N.withSocketsDo $ do
            inSock <- N.listenOn $ N.PortNumber $ fromIntegral cp
            (inHandle,_,_)  <- N.accept inSock
            msg <- hGetLine inHandle
            N.sClose inSock
            return msg
       ss v = N.withSocketsDo $ do
            _ <- forkIO $ do
                outHandle <- waitForConnect hostName $ N.PortNumber $ port hostPort
                hPrint outHandle v
            return ()
       (hostName, _:hostPort) = break (==':') host
       ex = zipWith (\a b -> (a ++ dataBreak : b))  ["host","clientPort","type"] [host,show cp,t]

newLocalChan :: String -> Integer -> IO Channel
newLocalChan t cp = N.withSocketsDo $ do
    hanVar <- newEmptyMVar
    _ <- forkIO $ do
        inSock <- N.listenOn $ N.PortNumber $ fromIntegral cp
        (inHandle,_,_)  <- N.accept inSock
        putMVar hanVar inHandle
    return $ Channel t cp (send' hanVar) (receive' hanVar) ex
      where
       ex = zipWith (\a b -> (a ++ dataBreak : b))  ["host","clientPort","type"] ["localhost",show cp,t]

newForeignChan :: String -> String -> IO Channel
newForeignChan t host = N.withSocketsDo $ do
    hanVar <- newEmptyMVar
    _ <- forkIO $ do
        outHandle <- waitForConnect hostName $ N.PortNumber $ port hostPort
        putMVar hanVar outHandle
    return $ Channel t 0 (send' hanVar) (receive' hanVar) ex
    where
       (hostName, _:hostPort) = break (==':') host
       ex = zipWith (\a b -> a ++ dataBreak : b)  ["host","clientPort","type"] [host,"0",t]

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
        printH han
        putStrLn $ "sending " ++ show v
        hPutStrLn han v
        putMVar hanVar han

receive' :: MVar Handle -> IO String
receive' hanVar = do
        han <- readMVar hanVar
        printH han
        msg <- hGetLine han
        putStrLn $ "receiving " ++ msg
        return msg

printH :: Handle -> IO ()
printH h = do
   hstr <- hShow h
   putStrLn hstr
