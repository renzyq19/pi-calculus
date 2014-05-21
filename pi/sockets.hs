import Network
import Control.Concurrent
import System.IO
import Network.HTTP.Base
import Network.URI
import Data.Maybe
import Channel
 
main :: IO ()
main = do
    hVar <- newEmptyMVar
    forkIO $ do
        handle <- connectTo "www.google.com" (PortNumber 80)
        putMVar hVar handle
    forkIO $ do
        handle <- takeMVar hVar
        hPutStr handle $ fromJust $ httpGetRequest "http://google.com"
        hFlush handle
        putMVar hVar handle
    forkIO $ do
        handle <- readMVar hVar
        msg <- hGetContents handle
        putStrLn msg
    return ()

httpGetRequest :: String -> Maybe String
httpGetRequest str = do
        uri <- parseURI str
        return $ show (mkRequest GET uri :: Request String)

local :: IO ()
local = do
    chan <- newLocalChan "" 8000
    _ <- receive chan
    send chan "Thanks for that"
    
foreig :: IO ()
foreig = do
    chan <- newForeignChan "" "localhost:8000"
    send chan "you look nice"
    receive chan >>= putStrLn
    
