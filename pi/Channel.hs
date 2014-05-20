module Channel where

import qualified Network as N
import System.IO (Handle, hFlush, hGetContents, hGetLine, hPrint, hShow, stderr, stdin, stdout)
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
        write = hPrint h
        rd = hGetLine h

newChan :: String -> String -> Integer -> IO Channel
newChan t host cp = return $ Channel t cp ss rr ex
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
                where waitForConnect h p = N.connectTo h p `catchIOError` (\_ -> do
                                                                threadDelay 10000
                                                                putStrLn "waiting for connection"
                                                                waitForConnect h p)
       port = fromIntegral . read
       (hostName, _:hostPort) = break (==':') host
       ex = zipWith (\a b -> (a ++ dataBreak : b))  ["host","clientPort","type"] [host,show cp,t]
                   
newExternChan :: String -> String -> Integer -> IO Channel
newExternChan t host cp = N.withSocketsDo $ do
    hanVar <- newEmptyMVar
    _ <- forkIO $ do 
        outHandle <- waitForConnect hostName $ N.PortNumber $ port hostPort 
        putMVar hanVar outHandle 
    let s v = do 
        han <- takeMVar hanVar
        printH han
        putStrLn $ "sending " ++ show v 
        hPrint han v
        hFlush han
        putMVar hanVar han
    let r = do
        han <- readMVar hanVar
        printH han
        msg <- hGetContents han
        putStrLn $ "receiving " ++ msg
        return msg 
    return $ Channel t cp s r ex
    where
       waitForConnect h p = N.connectTo h p `catchIOError` 
                                    (\_ -> do
                                        threadDelay 10000
                                        putStrLn "waiting for connection"
                                        waitForConnect h p)
       port = fromIntegral . read
       (hostName, _:hostPort) = break (==':') host
       ex = zipWith (\a b -> a ++ dataBreak : b)  ["host","clientPort","type"] [host,show cp,t]
       
dataBreak :: Char
dataBreak = '#'

printH :: Handle -> IO ()
printH h = do
   hstr <- hShow h
   putStrLn hstr
