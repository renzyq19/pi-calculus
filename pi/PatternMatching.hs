module PatternMatching (match) where            
            
import Control.Arrow (second) 
import Control.Monad (liftM,liftM2)
import Control.Monad.Error (throwError)
import TypDefs
--import Parser (readTerm)
import Network.HTTP.Base 

match :: Term -> Term -> ThrowsError [(Name,Value)]
match a b = liftM (map (second Term)) $ match' a b

match' :: Term -> Term -> ThrowsError [(Name,Term)]
match' (TVar name _) term = case name of
                                '_':_ -> return []
                                _     -> return [(name,term)]
match' (TPair (m1, m2)) (TPair (t1,t2)) = liftM2 (++) (match' m1 t1)  (match' m2 t2)
match' (TList (m:ms)) (TList (t:ts)) = do
                bind <- match' m t
                rest <- case ms of
                    [v] -> match' v $ TList ts
                    _   -> match' (TList ms) (TList ts)
                return $ bind ++ rest
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

{-
matchTest :: String -> String -> ThrowsError [(String,Value)]
matchTest s1 s2 = do
    t1 <- readTerm s1
    t2 <- readTerm s2
    match t1 t2
    -}
    
