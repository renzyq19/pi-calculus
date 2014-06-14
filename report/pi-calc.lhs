%include polycode.fmt

\begin{code}
data PiProcess = 
\end{code}

Our data type representing a process

\begin{code}
                 In String String         
\end{code}

Wait for message in on Channel and assign it to Variable

\begin{code}
               | Out String String
\end{code}

Send out a message on a channel

\begin{code}
               | Conc [PiProcess] 
\end{code}

Perform a list of Processes concurrently

\begin{code}
               | PiProcess `Seq` PiProcess
\end{code}

Perform two Processes sequentially

We can then define how our data type is to be printed, here we choose to make it identical to the original input for readability:

\begin{code}
instance Show PiProcess where
    show (In c m) =  "in(" ++ c ++ "," ++ m ++ ")"
    show (Out c m) =  "out(" ++ c ++ "," ++  m ++ ")"
    show (Conc procs) = intercalate "|" $ map show procs
    show (p1 `Seq` p2) = show p1 ++ ";" ++ show p2 
\end{code}
