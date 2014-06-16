import Control.Monad (forever,unless)
import Control.Concurrent (forkFinally, forkIO)
import Control.Monad.IO.Class (liftIO)
import System.IO (hGetLine, hPutStrLn)
import qualified Network as N


-- SERVER Process Code
server :: IO ()
server = do
    sock <- N.listenOn $ N.PortNumber 9000 
    (handle,_,_) <- N.accept sock
    _ <- forkFinally (forever $ do 
        msg <- hGetLine handle
        hPutStrLn handle msg) (\_ -> N.sClose sock)
    return ()

-- CLIENT Process Code
client :: IO ()
client = do 
    handle <- N.connectTo "localhost" $ N.PortNumber 9000
    let loop = do
            line <- getLine 
            hPutStrLn handle line
            msg <- hGetLine handle
            putStrLn msg
            loop
    loop

main :: IO ()
main = do
    _ <- forkIO $ server
    client
