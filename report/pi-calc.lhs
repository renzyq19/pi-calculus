%include polycode.fmt

>module PiProcess where

>import Data.List
>data PiProcess = 

Our abstract data type

>          In  Name Variable         |

Wait for message in on Channel and assign it to Variable

>          Out  Name Term        |

Send out Message on Channel

>          PiProcess `Conc`  PiProcess    |

Perform Processes concurrently

>          PiProcess `Seq` PiProcess    |

Perform Processes sequentially

>          Repl PiProcess              |

Replicate process

>          If Condition PiProcess PiProcess |

Conditional selection of process

>          New Name                    |

Creation of new / reserved name in proceeding processes.

>          Let Name Term PiProcess 

Pattern match name with a term in process

>          deriving (Eq)

We can define our terms as follows:

>data Term = TName Name         |

Plain name

>            TVar  Variable     |    

Plain variable (we keep the distinction for now to be in keeping with \cite{af01} and \cite{rs13})

>            TFun  Name [Term] Int   

Functions over a list of terms, holding information about their arity 

>               deriving (Eq)

To begin with, for the purposes of getting a basic parser up and running, we will simply have Variabls and Names be type synonyms for String. 

>type Variable   = String
>type Name      = String

This will change in future, as both will eventually need to contain some type information. This should be achievable by making them data types of their own.

Condition currently only holds information about two terms being equal

>data Condition = Term `Equals` Term deriving (Eq, Show)

This may change 

We can then define how our data type is to be printed, here we choose to make it identical to the original input for readability:

>instance Show PiProcess where
>    show (In c m) =  "in(" ++ c ++ "," ++ m ++ ")"
>    show (Out c m) =  "out(" ++ c ++ "," ++  show m ++ ")"
>    show (Repl proc) =  "!(" ++ show proc ++ ")"
>    show (p1 `Conc` p2) = show p1 ++ "|\n" ++ show p2 
>    show (p1 `Seq` p2) = show p1 ++ ";\n" ++ show p2 
>    show (New n)   = "new " ++ n
>    show (If c p1 p2) = "if " ++ show c ++ " then " ++ show p1 ++ " else " ++ show p2
>    show (Let n t p2) = "let " ++ n ++ " = " ++ show t ++ " in\n" ++ show p2 

>instance Show Term where
>   show (TVar x) = x
>   show (TName n)= n
>   show (TFun n [] 0) = n
>   show (TFun n ts _) = n ++ "(" ++ (concat (intersperse "," (map show ts))) ++ ")"

