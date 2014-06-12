module Primitives (primitives) where

import Control.Applicative ((<$>))
import Control.Monad.Error (throwError)
import Network.HTTP.Base (Request(..), Response(..), RequestMethod(..), mkRequest)
import Network.HTTP.Cookie (Cookie, cookiesToHeader)
import Network.HTTP.Headers (HasHeaders(..), Header, HeaderName(..), hdrName, hdrValue, findHeader, parseHeader, replaceHeader, setHeaders)
import Network.URI (parseURI)

import TypDefs

primitives :: [(String      , TermFun)]
primitives = [ ("fst"       , first)
             , ("snd"       , secnd)
             , ("hash"      , unaryId "hash")
             , ("pk"        , unaryId "pk")
             , ("httpReq"   , httpReq)
             , ("httpResp"  , httpResp)
             , ("getmsg"    , getmsg)
             , ("pair"      , binaryId "pair")
             , ("sdec"      , sdec)
             , ("senc"      , binaryId "senc")
             , ("adec"      , adec)
             , ("aenc"      , binaryId "aenc")
             , ("sign"      , binaryId "sign")
             , ("checksign" , checksign)
             , ("mac"       , binaryId "mac")
             , ("headers"   , listId)
             , ("header"    , header)
             , ("cookies"   , listId)
             , ("httpGet"   , constId "httpGet")
             , ("httpHead"  , constId "httpHead")
             , ("httpPost"  , constId "httpPost")
             , ("getHeaders", getHeadersFromData)
             , ("setHeader" , setHeader)
             , ("getCookie" , getCookie)
             , ("setCookie" , setCookie)
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
httpResp [TStr code, TStr reason , TList hs ,TStr bdy] = do
        headers <- makeHeaders hs
        return $ TData $ Resp $ Response (parseCode code) reason headers bdy
        where parseCode c =  case map (read . return) c of 
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

setHeader :: TermFun
setHeader [h@(TStr _), TData d] = do 
                      headr <- makeHeader h
                      let name = hdrName headr
                      let val  = hdrValue headr
                      return $ TData $ replaceHeader name val d 
setHeader [h@(TStr _), TList hs] = return $ TList $ h : hs  
setHeader e = throwError $ TypeMismatch "(header,httpData|headers)" $ map Term e

getCookie :: TermFun
getCookie [TData d] = 
    case findHeader HdrCookie d of
        Just x -> return $ TStr x
        Nothing -> throwError $ Default "cookie not found"
getCookie e = throwError $ TypeMismatch "httpData" $ map Term e
    
setCookie :: TermFun
setCookie [TData d, c] = do
    cookie <- makeCookie c
    let cookieHeader = cookiesToHeader [cookie]
    let n = hdrName cookieHeader
    let s = hdrValue cookieHeader
    return $ TData $ replaceHeader n s d 
setCookie e = throwError $ TypeMismatch "(httpData, cookie)" $ map Term e


