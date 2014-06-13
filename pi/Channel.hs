module Channel  (
    stdChan        ,
    newChan        ,
    newDummyChan   ,
    serialisable   ,  
    getChannelData )
    where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.MVar
import qualified Control.Concurrent.Chan as Ch
import Network.BSD (getHostName)
import qualified Network as N
import System.IO (BufferMode(..), Handle, hFlush, hGetLine, hPutStr, hPutStrLn, hReady, hSetBuffering)
import System.IO.Error (catchIOError)

import TypDefs (Channel (..), BuildType (..))

stdChan :: Handle -> Channel
stdChan h = Channel write rd []
    where
        write = hPutStrLn h
        rd = hGetLine h

newChan :: BuildType -> String -> Integer -> IO Channel
newChan t host port =
            case t of
                Init    -> newChanServer port
                Connect -> newChanClient host port

newDummyChan :: IO Channel
newDummyChan = do
    chan <- Ch.newChan
    return $ Channel (Ch.writeChan chan) (Ch.readChan chan) []
    
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
        (clientHandle,_,_)  <- N.accept sock
        putMVar hanVar clientHandle
    currentHost <- getHostName
    let ex = makeExtra [hostSig,portSig] [currentHost, show cp]
    return $ Channel (send' hanVar) (receive' hanVar) ex

newChanClient :: String -> Integer -> IO Channel
newChanClient hostName hostPort = N.withSocketsDo $ do
    hanVar <- newEmptyMVar
    _ <- forkIO $ do
        serverHandle <- waitForConnect hostName $ N.PortNumber $ fromIntegral hostPort
        putMVar hanVar serverHandle
    return $ Channel (send' hanVar) (receive' hanVar) ex
    where
       ex = makeExtra [hostSig, portSig] [hostName,  show hostPort]

waitForConnect :: N.HostName -> N.PortID -> IO Handle
waitForConnect h p = N.connectTo h p `catchIOError`
                                    (\_ -> do
                                        threadDelay 1000000
                                        putStrLn "waiting for connection"
                                        waitForConnect h p)

send' :: MVar Handle -> String -> IO ()
send' hanVar msg = do
        han <- takeMVar hanVar
        hPutStrLn han msg
        putMVar hanVar han

receive' :: MVar Handle -> IO String
receive' hanVar = do
        han <- readMVar hanVar
        unlines <$> emptyHandle han

emptyHandle :: Handle -> IO [String]
emptyHandle h = do
    line <- hGetLine h
    more <- hReady h `catchIOError` (\_ -> return False)
    if more
        then (line:) <$> emptyHandle h
        else return [line]

--lineBuffer :: Handle -> IO ()
--lineBuffer h = hSetBuffering h LineBuffering

dataBreak :: Char
dataBreak = '#'

hostSig :: String
hostSig = "host"

portSig :: String
portSig = "port"

makeExtra :: [String] -> [String] -> [String]
makeExtra = zipWith (\a b -> (a ++ dataBreak : b))

serialisable :: Channel -> Bool
serialisable = not . null . extra

getChannelData :: [String] -> Maybe (String, Integer)
getChannelData strs = do
        let ex = map (second tail . break (==dataBreak)) strs
        host         <- lookup hostSig ex
        port         <- lookup portSig ex
        return (host,read port)
