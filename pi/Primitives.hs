module Primitives (primitives) where

import Control.Applicative ((<$>))
import Control.Monad.Error (throwError)
import qualified Crypto.Hash as Crypto
import Data.Byteable (toBytes)
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Base (Request(..), Response(..), RequestMethod(..), mkRequest)
import Network.HTTP.Cookie (Cookie, cookiesToHeader)
import Network.HTTP.Headers (HasHeaders(..), Header, HeaderName(..), hdrName, hdrValue, findHeader, parseHeader, replaceHeader, setHeaders)
import Network.URI (parseURI)

import TypDefs

primitives :: [(String        , TermFun)]
primitives = [ ("fst"         , first)
             , ("snd"         , secnd)
             , ("hash"        , hash)
             , ("pk"          , unaryId "pk")
             , ("httpReq"     , httpReq)
             , ("httpResp"    , httpResp)
             , ("getmsg"      , getmsg)
             , ("sdec"        , sdec)
             , ("senc"        , binaryId "senc")
             , ("adec"        , adec)
             , ("aenc"        , binaryId "aenc")
             , ("sign"        , binaryId "sign")
             , ("checksign"   , checksign)
             , ("mac"         , mac)
             , ("headers"     , listId)
             , ("header"      , header)
             , ("cookies"     , listId)
             , ("httpGet"     , constId "httpGet")
             , ("httpHead"    , constId "httpHead")
             , ("httpPost"    , constId "httpPost")
             , ("getHeaders"  , getHeadersFromData)
             , ("insertHeader", insertHeader)
             , ("getHeader"   , getHeader)
             , ("getCookie"   , getCookie)
             , ("setCookie"   , setCookie)
             , ("uri"         , makeUri)
             , ("rspCode"     , responseCode)
             , ("add"         , add)
             , ("cons"        , cons)
             , ("head"        , head')
             , ("tail"        , tail')
             ]



constId :: String -> TermFun
constId name [] = return $ TFun name []
constId name e  = throwError $ NumArgs name 0 (map Term e)

unaryId :: String -> TermFun
unaryId name [x] =  return $ TFun name [x] 
unaryId name e  = throwError $ NumArgs name 1 (map Term e)

binaryId :: String ->  TermFun
binaryId name [x,y] = return $ TFun name [x,y] 
binaryId name e  = throwError $ NumArgs name 2 (map Term e)

listId :: TermFun
listId ts = return $ TList ts

getmsg :: TermFun
getmsg [TFun "sign" [_,y]] = return y
getmsg e = throwError $ TypeMismatch "sign" $ map Term e

first :: TermFun
first [TPair p] = return $ fst p
first e = throwError $ TypeMismatch "pair" $ map Term e 

secnd :: TermFun
secnd [TPair p] = return $ snd p
secnd e = throwError $ TypeMismatch "pair" $ map Term e 

sdec :: TermFun
sdec [k1, TFun "senc" [k2,y]]
    |k1 == k2  = return y
    |otherwise = throwError $ Default "keys not the same in sdec"
sdec e = throwError $ TypeMismatch "(var,senc(var,var))" $ map Term e

adec :: TermFun
adec [x , TFun "aenc" [TFun "pk" [k], y ]]
    | x == k = return y
    | otherwise= throwError $ Default "keys not same in adec" 
adec e = throwError $ TypeMismatch "(var,aenc(pk(var),var))" $ map Term e

checksign :: TermFun
checksign [TFun "pk" [k1], TFun "sign" [k2,_] ] = return $ TBool (k1 == k2)
checksign e = throwError $ TypeMismatch "(pk(var),sign(var,var))" $ map Term e


httpReq :: TermFun
httpReq [TStr url, TList hs , reqMethod] = do
    uri <- case parseURI http' of 
                Just x  -> return x
                Nothing -> throwError $ Default "Malformed uri"
    hss <- makeHeaders hs 
    let req = mkRequest method uri :: Request String in 
        return $ TData $ Req $ setHeaders req hss
    where
        http'  = if take 7 url /= http'' then http'' ++ url else url
        http'' = "http://" 
        method 
            | reqMethod == constFun "httpGet"  = GET 
            | reqMethod == constFun "httpHead" = HEAD 
            | reqMethod == constFun "httpPost" = POST 
            | otherwise = GET
            where constFun x = TFun x [] 
httpReq e = throwError $ TypeMismatch "(url,headers,method)" $ map Term e

httpResp :: TermFun
httpResp [TNum code, TStr reason , TList hs ,TStr bdy] = do
        headers <- makeHeaders hs
        return $ TData $ Resp $ Response (parseCode code) reason headers bdy
        where parseCode c =  case digits c of 
                                    [x,y,z] -> (x,y,z)
                                    _       -> (-1,-1,-1)
httpResp e = throwError $ TypeMismatch "(code,reason,headers,body)" $ map Term e

makeHeaders :: [Term] -> ThrowsError [Header]
makeHeaders = mapM makeHeader

makeHeader :: Term -> ThrowsError Header
makeHeader (TFun "cookies" cs) = do
        cookies <- mapM makeCookie cs 
        return $ cookiesToHeader cookies
makeHeader (TStr s) = case parseHeader s of
            Left _  -> throwError $ Default "malformed header"
            Right h -> return h
makeHeader _ = throwError $ Default "not a header"

header :: TermFun
header [TStr n , TStr v] = TStr . show <$> makeHeader (TStr (n ++ ":" ++ v))
header e = throwError $ TypeMismatch "(headerName, headerValue)" $ map Term e

makeCookie :: Term -> ThrowsError Cookie
makeCookie (TStr c) = return $ read c
makeCookie _ = throwError $ Default "malformed cookie"

getHeadersFromData :: TermFun
getHeadersFromData [TData d] = return $ TList $ map (TStr . show) $ getHeaders d
getHeadersFromData e = throwError $ TypeMismatch "httpData" $ map Term e

getHeader :: TermFun
getHeader [TStr headr, TData d] = do
        h <- case parseHeader $ headr ++ ":" of
            Right h -> return h
            _       -> throwError $ Default $ "Not a header: " ++ headr
        case findHeader (hdrName h) d of 
            Just hr -> return $ TStr hr
            _       -> throwError $ Default $ headr ++ " not in " ++ show d
getHeader e = throwError $ TypeMismatch "httpData" $ map Term e

insertHeader :: TermFun
insertHeader [h@(TStr _), TData d] = do 
                      headr <- makeHeader h
                      let name = hdrName headr
                      let val  = hdrValue headr
                      return $ TData $ replaceHeader name val d 
insertHeader [h@(TStr _), TList hs] = return $ TList $ h : hs  
insertHeader e = throwError $ TypeMismatch "(header,httpData|headers)" $ map Term e

getCookie :: TermFun
getCookie [TData d] = 
    case findHeader HdrCookie d of
        Just x -> return $ TStr x
        Nothing -> throwError $ Default "cookie not found"
getCookie e = throwError $ TypeMismatch "httpData" $ map Term e
    
setCookie :: TermFun
setCookie [c, TData d] = do
    cookie <- makeCookie c
    let cookieHeader = cookiesToHeader [cookie]
    let n = hdrName cookieHeader
    let s = hdrValue cookieHeader
    return $ TData $ replaceHeader n s d 
setCookie e = throwError $ TypeMismatch "(httpData, cookie)" $ map Term e

makeUri :: TermFun
makeUri [TStr host,TStr path] = return $ TStr $ host ++ "/" ++ path
makeUri e = throwError $ TypeMismatch "(string,string)" $ map Term e

responseCode :: TermFun
responseCode [TData d] = case d of
                        Req  _ -> throwError $ Default "Trying to extract Response Code from HttpRequest"
                        Resp r -> return $ TNum $ let (x,y,z) = rspCode r in fromIntegral (100 * x + 10 * y + z)
responseCode e = throwError $ TypeMismatch "(string,string)" $ map Term e

digits :: Integer -> [Int]
digits n = map (read . return) $ show n

add :: TermFun
add [TNum a, TNum b] = return $ TNum $ a + b
add e = throwError $ TypeMismatch "(num,num)" $ map Term e

cons :: TermFun
cons [a, TList x] = return $ TList (a:x)
cons e = throwError $ TypeMismatch "list" $ map Term e

head' :: TermFun
head' [TList []] = throwError $ Default "head of empty list" 
head' [TList xs] = return $ head xs
head' e = throwError $ TypeMismatch "list" $ map Term e

tail' :: TermFun
tail' [TList []] = throwError $ Default "tail of empty list" 
tail' [TList x] = return $ TList $ tail x
tail' e = throwError $ TypeMismatch "list" $ map Term e

hash :: TermFun
hash [TBS msg]  = return $ TBS $ toBytes (Crypto.hash msg :: Crypto.Digest Crypto.MD5)
hash [TStr msg] = let mBS = C8.pack msg in return $ TBS $ toBytes (Crypto.hash mBS :: Crypto.Digest Crypto.MD5)
hash e = throwError $ TypeMismatch "bytestring" $ map Term e
        
mac :: TermFun
mac [TBS key,TBS msg]  = return $ TBS $ toBytes $ Crypto.hmacGetDigest $ Crypto.hmacAlg Crypto.MD5 key msg
mac [TBS key,TStr msg] = let mBS = C8.pack msg in return $ TBS $ toBytes $ Crypto.hmacGetDigest $ Crypto.hmacAlg Crypto.MD5 key mBS
mac e = throwError $ TypeMismatch "(bytestring,bytestring)" $ map Term e
