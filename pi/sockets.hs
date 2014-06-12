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
    let msg = "Message"
    ch <- newChan Connect ("localhost:" ++ show n) 1
    send ch msg

server :: Int -> IO ()
server n = do
    ch <- newChan Init ("localhost:" ++ show n) (fromIntegral n)
    receive ch >>= putStrLn
