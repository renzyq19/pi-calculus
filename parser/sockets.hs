import Network
import System.IO
import Control.Concurrent

main :: IO ()
main = withSocketsDo $ do
    _ <- forkIO recv
    --_ <- forkIO send
    return ()

recv :: IO ()
recv = do
    s2 <- listenOn $ PortNumber 9000
    (h,_,_) <- accept s2
    str <- hGetLine h
    hShow h >>= putStrLn
    sClose s2
    putStrLn str

send :: IO ()
send = do 
    s1 <- connectTo "localhost" $ PortNumber 9000
    hPutStrLn s1 "hello"


