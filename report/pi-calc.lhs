%include polycode.fmt

>data PiProcess = 

Our data type representing a process

>                 In String String         

Wait for message in on Channel and assign it to Variable

>               | Out String String

Send out a message on a channel

>               | Conc [PiProcess] 

Perform a list of Processes concurrently

>               | PiProcess `Seq` PiProcess

Perform two Processes sequentially

We can then define how our data type is to be printed, here we choose to make it identical to the original input for readability:

>instance Show PiProcess where
>    show (In c m) =  "in(" ++ c ++ "," ++ m ++ ")"
>    show (Out c m) =  "out(" ++ c ++ "," ++  m ++ ")"
>    show (Conc procs) = intercalate "|" $ map show procs
>    show (p1 `Seq` p2) = show p1 ++ ";\n" ++ show p2 

