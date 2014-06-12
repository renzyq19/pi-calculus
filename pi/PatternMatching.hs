module PatternMatching (match) where            
            
import Control.Arrow (second) 
import Control.Monad (liftM,liftM2,zipWithM)
import Control.Monad.Error (throwError)
import TypDefs
import Parser ()
import Network.HTTP.Base 

match :: Term -> Term -> ThrowsError [(String,Value)]
match a b = liftM (map (second Term)) $ match' a b

match' :: Term -> Term -> ThrowsError [(String,Term)]
match' (TVar name _) term = return [(name,term)]
match' (TPair (m1, m2)) (TPair (t1,t2)) = liftM2 (++) (match' m1 t1)  (match' m2 t2)
match' (TList ms) (TList ts) = liftM concat $ zipWithM match' ms ts
match' l@(TList _) (TData d) = match' l $ dataToList d
match' t1 t2 = throwError $ PatternMatch t1 t2

dataToList :: HttpData -> Term
dataToList (Req r) = TList [TStr uri, TList headers, method]
    where
        uri = show $ rqURI r
        headers = map (TStr . show) $ rqHeaders r
        method = case rqMethod r of
                    HEAD -> TFun "httpHead" []
                    GET  -> TFun "httpGet"  []
                    POST -> TFun "httpPost" []
                    _    -> TFun "httpGet"  []
dataToList (Resp r) = TList [TNum code, TStr reason, TList headers, TStr bdy]
    where
        code = let (x,y,z) = rspCode r in fromIntegral (100*x + 10* y + z) 
        reason = rspReason r
        headers = map (TStr . show) $ rspHeaders r
        bdy = rspBody r

{-testMatch :: String -> String -> ThrowsError [(String,Term)]
testMatch s1 s2 = do
    t1 <- readTerm s1
    t2 <- readTerm s2
    match' t1 t2-}
    
