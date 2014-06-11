module Primitives (primitives) where

import Control.Monad.Error (throwError)
import Network.HTTP.Base (Request(..), Response(..), RequestMethod(..), mkRequest)
import Network.HTTP.Cookie (Cookie, cookiesToHeader)
import Network.HTTP.Headers (Header, HeaderName(..), hdrName, hdrValue, findHeader, parseHeader, replaceHeader, setHeaders)
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
             , ("cookies"   , listId)
             , ("httpGet"   , constId "httpGet")
             , ("httpHead"  , constId "httpHead")
             , ("httpPost"  , constId "httpPost")
             , ("getCookie" , getCookie)
             , ("setCookie" , setCookie)
             ]

constId :: String -> TermFun
constId name [] = return $ TFun name [] 0
constId name e  = throwError $ NumArgs name 0 (map Term e)

unaryId :: String -> TermFun
unaryId name [x] =  return $ TFun name [x] 1
unaryId name e  = throwError $ NumArgs name 1 (map Term e)

binaryId :: String ->  TermFun
binaryId name [x,y] = return $ TFun name [x,y] 2 
binaryId name e  = throwError $ NumArgs name 2 (map Term e)

listId :: TermFun
listId ts = return $ TList ts

getmsg :: TermFun
getmsg [TFun "sign" [_,y] 2] = return y
getmsg e = throwError $ TypeMismatch "sign" $ map Term e

first :: TermFun
first [TPair p] = return $ fst p
first e = throwError $ TypeMismatch "pair" $ map Term e 

secnd :: TermFun
secnd [TPair p] = return $ snd p
secnd e = throwError $ TypeMismatch "pair" $ map Term e 

sdec :: TermFun
sdec [k1, TFun "senc" [k2,y] 2]
    |k1 == k2  = return y
    |otherwise = throwError $ Default "keys not the same in sdec"
sdec e = throwError $ TypeMismatch "(var,senc(var,var))" $ map Term e

adec :: TermFun
adec [x , TFun "aenc" [TFun "pk" [k] 1, y ] 2]
    | x == k = return y
    | otherwise= throwError $ Default "keys not same in adec" 
adec e = throwError $ TypeMismatch "(var,aenc(pk(var),var))" $ map Term e

checksign :: TermFun
checksign [TFun "pk" [k1] 1 , TFun "sign" [k2,_] 2 ] = return $ TBool (k1 == k2)
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
            where constFun x = TFun x [] 0 
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
makeHeader (TFun "cookies" cs _) = do
        cookies <- mapM makeCookie cs 
        return $ cookiesToHeader cookies

makeHeader (TStr s) = case parseHeader s of
            Left _  -> throwError $ Default "malformed header"
            Right h -> return h
makeHeader _ = throwError $ Default "not a header"

makeCookie :: Term -> ThrowsError Cookie
makeCookie (TStr c) = return $ read c
makeCookie _ = throwError $ Default "malformed cookie"


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
