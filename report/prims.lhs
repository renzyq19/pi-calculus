%include polycode.fmt

>fst(pair(a,b)) => a

>snd(pair(a,b)) => b

>cons(x ,list(a,b..)) => list(x,a,b..)

>head(list(a,b..)) => a

>tail(list(a,b..)) => list(b..)

>httpReq(url, headers(..), method) => request : HttpRequest

>httpResp(responseCode, reason, headers(..), body) => response : HttpResponse

>headers(h1,h2..) => list(h1,h2..)

>header(name,value) => h : Header

>cookies(c1,c2..) => list(c1,c2..)

>getHeaders(httpData) => headers(..)

>setHeader(header,httpData) => httpData 
>setHeader(header,headers(h1,h2..)) => headers(header,h1,h2..))  

>getHeader(headerName, httpData) =>  headerVal

>getCookie(httpData) => httpData

>setCookie(cookie, httpData) => httpData

>uri(host,resource) => uri

>rspCode(response) => code

>add(num1, num2) => 

%TODO

>hash

>pk

>getmsg   

>sdec

>senc

>adec

>aenc

>sign

>checksign

>mac

