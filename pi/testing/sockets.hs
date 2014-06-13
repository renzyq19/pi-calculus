import Network.HTTP
import Network.URI
import Network.Browser
import Data.Maybe
import Channel
import TypDefs
import Control.Monad
import Control.Concurrent (forkIO)
 
main :: IO ()
main = do
    let port = 90010
    _ <- forkIO $ client port
    _ <- forkIO $ server port
    return ()

client :: Int -> IO ()
client n = do 
    let msg = "Ping!"
    ch <- newChan Connect "localhost" (fromIntegral n)
    send ch msg
    receive ch >>= putStrLn

server :: Int -> IO ()
server n = do
    let msg = "Pong!"
    ch <- newChan Init "localhost" (fromIntegral n)
    receive ch >>= putStrLn
    send ch msg
