module Channel  (
    stdChan        ,
    newChan        ,
    dataBreak      )
    where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.MVar
import Network.BSD (getHostName)
import qualified Network as N
import System.IO (Handle, hFlush, hGetLine, hPutStr, hPutStrLn, hReady)
import System.IO.Error (catchIOError)

import TypDefs (Channel (..), ChannelType (..))

stdChan :: Handle -> Channel
stdChan h = Channel write rd False []
    where
        write = hPutStrLn h
        rd = unlines <$> emptyHandle h

newChan :: ChannelType -> String -> Integer -> IO Channel
newChan t host cp = do
            currentHost <- getHostName
            case t of
                Internal -> newInternalChan currentHost hostPort cp
                HTTP
                    | hostName == "localhost" || hostName == currentHost -> newChanServer t cp
                    | otherwise               -> newChanClient t hostName hostPort
                _                             -> newChanClient t hostName hostPort
               where
               (hostName, _:hostPort) = break (==':') host

newInternalChan :: String -> String -> Integer -> IO Channel
newInternalChan hostName hostPort cp = return $ Channel s r True ex
    where
       r   = N.withSocketsDo $ do
            sock <- N.listenOn $ N.PortNumber $ fromIntegral cp
            (inHandle,_,_) <- N.accept sock
            msg <- unlines <$> emptyHandle inHandle
            N.sClose sock
            return msg
       s v = N.withSocketsDo $ do
            _ <- forkIO $ do
                outHandle <- waitForConnect hostName $ N.PortNumber $ port hostPort
                hPutStr outHandle v
            return ()
       ex = makeExtra ["host","clientPort","type"] [hostName ++ ":" ++ hostPort,show cp,show HTTP]


newChanServer :: ChannelType -> Integer -> IO Channel
newChanServer t cp = N.withSocketsDo $ do
    hanVar <- newEmptyMVar
    _ <- forkIO $ do
        sock <- N.listenOn $ N.PortNumber $ fromIntegral cp
        (inHandle,_,_)  <- N.accept sock
        putMVar hanVar inHandle
    currentHost <- getHostName
    let ex  = makeExtra ["clientPort","type"] [show cp,show t]
    let ex' = ex ++ makeExtra ["host"] [currentHost ++ ":" ++ show cp]
    return $ Channel (send' hanVar) (receive' hanVar) True ex'

newChanClient :: ChannelType -> String -> String -> IO Channel
newChanClient t hostName hostPort = N.withSocketsDo $ do
    hanVar <- newEmptyMVar
    _ <- forkIO $ do
        outHandle <- waitForConnect hostName $ N.PortNumber $ port hostPort
        putMVar hanVar outHandle
    return $ Channel (send' hanVar) (receive' hanVar) True ex
    where
       ex = makeExtra ["host","clientPort","type"] [hostName ++ ":" ++ hostPort,"-1",show t]

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
