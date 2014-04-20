%include polycode.ftm

\begin{code}
import Text.ParserCombinators.Parsec
import PiProcess


openB :: Parser Char
openB = char '(' 

closeB :: Parser Char
closeB = char ')' 

betweenB :: Parser [String]
betweenB = do{
            openB;
            [out] <- endBy line closeB;
            return out;
        } 
    where
        line = sepBy word (char ',')
        word = many $ noneOf ",)"

inOut :: Parser PiProcess
inOut = do {
        piIn;
        bContents <- betweenB;
        case bContents of
            [chan,message] -> return $ In chan message
            _ -> error (e "in(chan,message)")
        } <|> do {
        piOut;
        bContents <- betweenB;
        case bContents of
            [chan, message] -> return $ Out chan (TVar message)
            _ -> error (e "out(chan,message)")
        }
        where
            piIn= string "in"
            piOut=string "out" 
            e x= "malformed input " ++ x ++ " expected"
        
\end{code}
