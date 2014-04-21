module PiProcess where

import Data.List (intercalate)

data PiProcess = In   Name Variable
               | Out  Name Term
               | New  Name
               | PiProcess `Seq`   PiProcess -- Sequential Composition
               | PiProcess `Conc`  PiProcess -- Parallel   Composition
               | Replicate PiProcess         -- Infinite parallel replication
               | Let Name Term PiProcess
               | If Condition PiProcess PiProcess
                 deriving (Eq)

data Term = TName Name
          | TVar  Variable
          | TFun  Name [Term] Int
            deriving (Eq)

type Variable   = String
type Name      = String
data Condition = Term `Equals` Term deriving (Eq)

showPi :: PiProcess -> String
showPi (In c m) =  "in(" ++ c ++ "," ++ m ++ ")"
showPi (Out c m) =  "out(" ++ c ++ "," ++  show m ++ ")"
showPi (Replicate proc) =  "!(" ++ show proc ++ ")"
showPi (p1 `Conc` p2) = show p1 ++ "|\n" ++ show p2
showPi (p1 `Seq` p2) = show p1 ++ ";\n" ++ show p2 
showPi (New n)   = "new " ++ n
showPi (If c p1 p2) = "if " ++ show c ++ " then " ++ show p1 ++ " else " ++ show p2
showPi (Let n t p) = "let " ++ n ++ " = " ++ show t ++ " in\n" ++ show p

showTerm :: Term -> String
showTerm (TVar x) = x
showTerm (TName n)= n
showTerm (TFun n [] 0) = n
showTerm (TFun n ts _) = n ++ "(" ++ intercalate "," (map show ts) ++ ")"

showCond :: Condition -> String
showCond (t1 `Equals` t2) = show t1 ++ " == " ++ show t2


instance Show PiProcess where show = showPi
instance Show Term where show = showTerm
instance Show Condition where show = showCond
