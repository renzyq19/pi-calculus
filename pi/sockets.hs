import Network.HTTP
import Network.URI
import Network
import Channel
import System.IO
import Control.Monad
import Control.Concurrent

 
main :: IO ()
main = do
    _ <- forkIO $ do
        s <- listenOn $ PortNumber 9000
        (_,_,_) <- accept s
        sClose s
    h <- connectTo "localhost" (PortNumber 9000)
    hShow h >>= putStrLn
    return ()

httpGetRequest :: String -> Maybe String
httpGetRequest str = do
        uri <- parseURI str
        return $ show (mkRequest GET uri :: Request String)

receiveHttp :: Channel -> IO [String]
receiveHttp c = do
        l <- receive c
        liftM (l :) $ receiveHttp c
