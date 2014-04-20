%import polycode.ftm

\begin{code}
import Control.Monad (forever,unless)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS


-- SERVER Process Code
server :: IO ()
server = do
    WS.runServer "0.0.0.0" 9000 $ pong

pong :: WS.ServerApp
pong pending = do
    conn <- WS.acceptRequest pending
    forever $ do 
        msg <- WS.receiveData conn
        WS.sendTextData conn (msg :: T.Text)

-- CLIENT Process Code
client :: IO ()
client = WS.runClient "0.0.0.0" 9000 "/" ping

ping :: WS.ClientApp ()
ping conn = do
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg
    let loop = do
            line <- T.getLine
            unless (T.null line) $WS.sendTextData conn line
            loop
    loop

main :: IO ()
main = do
    _ <- forkIO $ server
    client
    


\end{code}
