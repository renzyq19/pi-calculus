import Network
import Control.Concurrent
import System.IO
import Network.HTTP.Base
import Network.URI
import Data.Maybe
 
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
