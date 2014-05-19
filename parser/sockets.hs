import Network
import Control.Concurrent
import System.IO
import Network.HTTP.Base
import Network.URI
import Data.Maybe
 
main :: IO ()
main = do
    forkIO $ do
        handle <- connectTo "www.google.com" (PortNumber 80)
        hPutStr handle $ fromJust $ httpGetRequest "http://google.com"
        hFlush handle
    return ()

httpGetRequest :: String -> Maybe String
httpGetRequest str = do
        uri <- parseURI str
        return $ show (mkRequest GET uri :: Request String)
