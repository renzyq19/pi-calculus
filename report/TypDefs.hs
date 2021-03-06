import Control.Monad.Trans.Except(ExceptT(..))
import Data.Char (toLower)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.List (intercalate)
import Data.Map (Map)
import Network.HTTP.Base (Request, Response)
import Network.HTTP.Headers (HasHeaders(..))

import Text.ParserCombinators.Parsec (ParseError)

data PiProcess = Null
               | In   Term Term
               | Out  Term Term
               | New  Term
               | PiProcess `Seq` PiProcess 
               | Conc [PiProcess]          
               | Replicate PiProcess       
               | Let Term Value (Maybe PiProcess)
               | If Condition PiProcess PiProcess
               | Atom Term
                 deriving (Eq)

data Term = TVar Name (Maybe Type)
          | TStr String
          | TNum Integer
          | TBool Bool
          | TPair (Term, Term)
          | TList [Term]
          | TBS ByteString
          | TData HttpData
          | TFun Name [Term]
            deriving (Eq)

data Condition = Term `Equals` Term deriving (Eq)

type Name      = String
data HttpData = Resp (Response String)
              | Req  (Request String)

instance HasHeaders HttpData where
    getHeaders (Resp r) = getHeaders r
    getHeaders (Req r)  = getHeaders r
    setHeaders (Resp r) = Resp . setHeaders r
    setHeaders (Req r)  = Req . setHeaders r


type TermFun = [Term] -> ThrowsError Term
type IOThrowsError = ExceptT PiError IO 
type ThrowsError   = Either  PiError

data Value = Proc PiProcess 
           | Term Term
           | Chan Channel
           | PrimitiveFunc TermFun
           | Func {params :: [String] , body :: Value, closure :: Env}


data PiError = NumArgs Name Integer [Value]
             | TypeMismatch String [Value]
             | Parser ParseError
             | UnboundVar String String
             | NotTerm Name Value
             | NotFunction String String
             | NotChannel String
             | NotProcess String
             | PatternMatch Term Term
             | Default String

data Type = HttpRequest
          | HttpResponse
          deriving (Eq, Read, Show)

type Env = IORef (Map String Value)

data Channel = Channel {
               send         :: String -> IO ()
             , receive      :: IO String
             , extra        :: [String]
             }

data BuildType = Init
               | Connect
                 deriving (Eq, Show, Read)

instance Show PiProcess where show = showPi
instance Show Term      where show = showTerm
instance Show Value     where show = showValue
instance Show Condition where show = showCond
instance Show PiError where show = showError
instance Show HttpData where show = showHttpData

showPi :: PiProcess -> String
showPi Null = "0"
showPi (In c m) =  "in(" ++ show c ++ "," ++ show m ++ ")"
showPi (Out c m) =  "out(" ++ show c ++ "," ++  show m ++ ")"
showPi (Replicate proc) =  "!(" ++ show proc ++ ")"
showPi (Conc procs) = intercalate "|" $ map show procs 
showPi (p1 `Seq` Null) = show p1
showPi (p1 `Seq` p2) = show p1 ++ ";" ++ show p2 
showPi (New n)   = "new " ++ show n
showPi (If c p1 Null) = "if " ++ show c ++ " then " ++ show p1 
showPi (If c p1 p2)   = "if " ++ show c ++ " then " ++ show p1 ++ " else " ++ show p2
showPi (Let n t p)    = "let " ++ show n ++ " = " ++ show t ++ case p of {Nothing -> "" ; Just x -> " in\n" ++ show x}
showPi (Atom t)       = show t

showTerm :: Term -> String
showTerm (TVar x t) = x ++ (case t of 
                                Nothing -> ""
                                Just ty -> ": " ++ show ty)
showTerm (TStr str) = show str
showTerm (TNum num) = show num
showTerm (TBool b ) = map toLower $ show b
showTerm (TList ls) = "list(" ++ intercalate "," (map show ls) ++ ")"
showTerm (TPair (a,b)) = "pair("++ show a ++ ","++ show b ++ ")"
showTerm (TData d) = show d
showTerm (TBS bs)  = show bs
showTerm (TFun n ts) = n ++ "(" ++ intercalate "," (map show ts) ++ ")"

showValue :: Value -> String
showValue (Proc p)  = show p
showValue (Term t)  = show t
showValue (Chan c)  = show $ convert c
    where 
        convert ch = TFun "<chan>" (map TStr ex) 
            where ex = extra ch
showValue (PrimitiveFunc _)  = "<primitive>" 
showValue (Func {})          = "<user function>"  

showCond :: Condition -> String
showCond (t1 `Equals` t2) = show t1 ++ " = " ++ show t2

showError :: PiError -> String
showError (UnboundVar message var)      = message ++ ": " ++ var
showError (NotFunction message fun)     = message ++ ": " ++ fun
showError (NotChannel chan)             = "Not a channel: " ++ chan 
showError (NotProcess proc)             = "Not a Process: " ++ proc
showError (NotTerm name var)            = "Expecting " ++ name ++ " to be a Term, found: " ++ show var
showError (NumArgs name expected found) = "Expected " ++ show name ++ show expected ++ " args; found values "
                                          ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found "
                                          ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (PatternMatch t1 t2)          = "Pattern match failure: cannot match " ++ show t1 ++ " and \n" ++ show t2
showError (Default msg)                 = msg

showHttpData :: HttpData -> String
showHttpData (Resp r) = show r
showHttpData (Req r)  = show r 

instance Eq Value where (==) = eqvVal
instance Eq HttpData where (==) = eqHttpData

eqHttpData :: HttpData -> HttpData -> Bool
eqHttpData _ _ = False

eqvVal :: Value -> Value -> Bool
eqvVal (Proc p1)  (Proc p2) = p1 == p2
eqvVal (Term t1)  (Term t2) = t1 == t2
eqvVal _ _ = False


unwordsList :: [Value] -> String
unwordsList = unwords . map show
