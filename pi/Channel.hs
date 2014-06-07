module Channel  (
    stdChan        ,
    newChan        ,
    newDummyChan   ,
    dataBreak      )
    where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.MVar
import qualified Control.Concurrent.Chan as Ch
import Network.BSD (getHostName)
import qualified Network as N
import System.IO (Handle, hFlush, hGetLine, hPutStr, hPutStrLn, hReady)
import System.IO.Error (catchIOError)

import TypDefs (Channel (..), BuildType (..))

stdChan :: Handle -> Channel
stdChan h = Channel write rd False []
    where
        write = hPutStrLn h
        rd = hGetLine h

newChan :: BuildType -> String -> Integer -> IO Channel
newChan t host cp =
            case t of
                Init    -> newChanServer cp
                Connect -> newChanClient hostName hostPort
               where
               (hostName, _:hostPort) = break (==':') host

newDummyChan :: IO Channel
newDummyChan = do
    chan <- Ch.newChan
    return $ Channel (Ch.writeChan chan) (Ch.readChan chan) False []
    
{-newInternalChan :: String -> String -> Integer -> IO Channel
newInternalChan hostName hostPort cp = do
    currentHost <- getHostName
    let ex = makeExtra ["host","clientPort"] [currentHost ++ ":" ++ hostPort,show cp]
    return $ Channel s r True ex
    where
       r   = N.withSocketsDo $ do
            sock <- N.listenOn $ N.PortNumber $ fromIntegral cp
            (inHandle,_,_) <- N.accept sock
            msg <- unlines <$> emptyHandle inHandle
            putStrLn $ "got: " ++ msg
            N.sClose sock
            return msg
       s v = N.withSocketsDo $ do
            _ <- forkIO $ do
                outHandle <- waitForConnect hostName $ N.PortNumber $ port hostPort
                putStrLn $ "sending: " ++ v
                hPutStr outHandle v
            return ()-}


newChanServer :: Integer -> IO Channel
newChanServer cp = N.withSocketsDo $ do
    hanVar <- newEmptyMVar
    _ <- forkIO $ do
        sock <- N.listenOn $ N.PortNumber $ fromIntegral cp
        (inHandle,_,_)  <- N.accept sock
        putMVar hanVar inHandle
    currentHost <- getHostName
    let ex  = makeExtra ["clientPort"] [show cp]
    let ex' = ex ++ makeExtra ["host"] [currentHost ++ ":" ++ show cp]
    return $ Channel (send' hanVar) (receive' hanVar) True ex'

newChanClient :: String -> String -> IO Channel
newChanClient hostName hostPort = N.withSocketsDo $ do
    hanVar <- newEmptyMVar
    _ <- forkIO $ do
        outHandle <- waitForConnect hostName $ N.PortNumber $ port hostPort
        putMVar hanVar outHandle
    return $ Channel (send' hanVar) (receive' hanVar) True ex
    where
       ex = makeExtra ["host","clientPort"] [hostName ++ ":" ++ hostPort,"-1"]

waitForConnect :: N.HostName -> N.PortID -> IO Handle
waitForConnect h p = N.connectTo h p `catchIOError`
                                    (\_ -> do
                                        threadDelay 10000
                                        putStrLn "waiting for connection"
                                        waitForConnect h p)

send' :: MVar Handle -> String -> IO ()
send' hanVar msg = do
        han <- takeMVar hanVar
        hPutStr han msg
        hFlush han
        putMVar hanVar han

receive' :: MVar Handle -> IO String
receive' hanVar = do
        han <- readMVar hanVar
        unlines <$> emptyHandle han

emptyHandle :: Handle -> IO [String]
emptyHandle h = do
    line <- hGetLine h
    more <- hReady h
    if not more
        then return []
        else (line:) <$> emptyHandle h

port :: String -> N.PortNumber
port s = fromIntegral (read s :: Integer)

dataBreak :: Char
dataBreak = '#'

makeExtra :: [String] -> [String] -> [String]
makeExtra = zipWith (\a b -> (a ++ dataBreak : b))
