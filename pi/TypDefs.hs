module TypDefs (
    PiProcess  (..),
    Term       (..),
    TermFun        ,
    Condition  (..),
    Value      (..),
    Channel    (..),
    Type       (..),
    ChannelType(..),
    PiError    (..),
    IOThrowsError  ,
    ThrowsError    ,
    Name           , 
    Env            )
    where

import Control.Monad.Trans.Except(ExceptT(..))
import Data.Char (toLower)
import Data.IORef (IORef)
import Data.List (intercalate)
import Data.Map (Map)

import Text.ParserCombinators.Parsec (ParseError)

data PiProcess = Null
               | In   Term Term
               | Out  Term Term
               | New  Term
               | PiProcess `Seq`   PiProcess -- Sequential Composition
               | Conc [PiProcess]            -- Parallel   Composition
               | Replicate PiProcess         -- Infinite parallel replication
               | Let Term Value (Maybe PiProcess)
               | If Condition PiProcess PiProcess
               | Atom Term
                 deriving (Eq)

data Term = TVar Name
          | TStr String
          | TNum Integer
          | TBool Bool
          | TPair (Term, Term)
          | TFun Name [Term] Int
            deriving (Eq)

type TermFun = [Term] -> ThrowsError Term

type Name      = String
data Condition = Term `Equals` Term deriving (Eq)

data Value = Proc PiProcess 
           | Term Term
           | Chan Channel
           | PrimitiveFunc TermFun
           | Func {params :: [String] , body :: Value, closure :: Env}

type IOThrowsError = ExceptT PiError IO 
type ThrowsError   = Either  PiError

data PiError = NumArgs Name Integer [Value]
             | TypeMismatch String [Value]
             | Parser ParseError
             | UnboundVar String String
             | NotTerm Name Value
             | NotFunction String String
             | NotChannel String
             | NotProcess String
             | Default String

instance Show PiError where show = showError

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
showError (Default msg)                 = msg

data Type = Blob
          | Text
          | HTTPData
          | Process
          | ChannelOf Type
           deriving (Eq, Show)


type Env = IORef (Map String Value)

data Channel = Channel {
               send         :: String -> IO ()
             , receive      :: IO String
             , serialisable :: Bool 
             , extra        :: [String]
             }

data ChannelType = Internal
                 | Std
                 | HTTP
                 | String
                 deriving (Eq, Show, Read)

showValue :: Value -> String
showValue (Proc p)  = show p
showValue (Term t)  = show t
showValue (Chan c)  = show $ convert c
    where 
        convert ch = TFun "<chan>" (map TStr ex) $ length ex
            where ex = extra ch
showValue (PrimitiveFunc _)  = "<primitive>" 
showValue (Func {})          = "<user function>"  

eqvVal :: Value -> Value -> Bool
eqvVal (Proc p1)  (Proc p2) = p1 == p2
eqvVal (Term t1)  (Term t2) = t1 == t2
eqvVal _ _ = False

instance Eq Value where (==) = eqvVal

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
showTerm (TVar x)   = x
showTerm (TStr str) = "\"" ++ str ++ "\""
showTerm (TNum num) = show num
showTerm (TBool b ) = map toLower $ show b
showTerm (TPair (a,b)) = "pair("++ show a ++ ","++ show b ++ ")"
showTerm (TFun n ts _ ) = n ++ "(" ++ intercalate "," (map show ts) ++ ")"

showCond :: Condition -> String
showCond (t1 `Equals` t2) = show t1 ++ " == " ++ show t2

instance Show PiProcess where show = showPi
instance Show Term      where show = showTerm
instance Show Condition where show = showCond
instance Show Value     where show = showValue

unwordsList :: [Value] -> String
unwordsList = unwords . map show
