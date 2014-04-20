module PiProcess where

import Data.List (intercalate)

data PiProcess = In   Name Variable
               | Out  Name Term
               | PiProcess `Conc`  PiProcess
               | PiProcess `Seq`   PiProcess
               | Repl PiProcess
               | If Condition PiProcess PiProcess
               | New Name
               | Let Name Term PiProcess
                 deriving (Eq)

data Term = TName Name
          | TVar  Variable
          | TFun  Name [Term] Int   
            deriving (Eq)
               
type Variable   = String
type Name      = String
data Condition = Term `Equals` Term deriving (Eq, Show)

showPi :: PiProcess -> String
showPi (In c m) =  "in(" ++ c ++ "," ++ m ++ ")"
showPi (Out c m) =  "out(" ++ c ++ "," ++  show m ++ ")"
showPi (Repl proc) =  "!(" ++ show proc ++ ")"
showPi (p1 `Conc` p2) = show p1 ++ "|\n" ++ show p2 
showPi (p1 `Seq` p2) = show p1 ++ ";\n" ++ show p2 
showPi (New n)   = "new " ++ n
showPi (If c p1 p2) = "if " ++ show c ++ " then " ++ show p1 ++ " else " ++ show p2
showPi (Let n t p2) = "let " ++ n ++ " = " ++ show t ++ " in\n" ++ show p2 

showTerm :: Term -> String
showTerm (TVar x) = x
showTerm (TName n)= n
showTerm (TFun n [] 0) = n
showTerm (TFun n ts _) = n ++ "(" ++ intercalate "," (map show ts) ++ ")"

instance Show PiProcess where show = showPi
instance Show Term where show = showTerm
   
