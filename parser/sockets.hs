import Network
import System.IO
import Control.Concurrent

main :: IO ()
main = withSocketsDo $ do
    forkIO $ do s1 <- connectTo "localhost" $ PortNumber 9000
                hPutStrLn s1 "hello"
    s2 <- listenOn $ PortNumber 9000
    (h,_,_) <- accept s2
    str <- hGetLine h
    putStrLn str


