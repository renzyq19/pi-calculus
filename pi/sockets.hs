import Network.HTTP
import Network.URI
import Data.Maybe
import Channel
import Control.Monad
 
main :: IO ()
main = do
    c <- newChan HTTP "www.google.com:80" 8000 
    let req = fromJust $ httpGetRequest "http://www.google.com/index.html"
    send c req 
    msg <- receive c
    print $ length msg
    print $ lines msg
    case parseResponseHead (lines msg) of
        Left _ -> error "no parse"
        Right rsp -> print rsp
    return ()

httpGetRequest :: String -> Maybe String
httpGetRequest str = do
        uri <- parseURI str
        return $ show (mkRequest GET uri :: Request String)

receiveHttp :: Channel -> IO [String]
receiveHttp c = do
        l <- receive c
        liftM (l :) $ receiveHttp c


    
    

    
