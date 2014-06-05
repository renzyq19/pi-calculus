module Parser (
        parseTerm,
        parseProcess)
        where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec

import TypDefs (Condition (..), PiProcess (..), Term (..), Value(..), Name)

parseNull :: Parser PiProcess
parseNull = do
            paddedChar '0'
            return Null

parseIn :: Parser PiProcess
parseIn = do
            _ <- string "in("
            name <- parseTerm
            paddedComma
            var  <- parseTerm
            _ <- char ')'
            parseSeq $ In name var 

parseOut :: Parser PiProcess
parseOut = do
            _ <- string "out("
            name <- parseTerm
            paddedComma
            term  <- parseTerm
            _ <- char ')'
            parseSeq $ Out name term 

parseReplicate :: Parser PiProcess
parseReplicate = do
            _ <- string "!("
            process <- parseProcess
            _ <- char ')'
            return $ Replicate process

paddedChar :: Char ->  Parser ()
paddedChar ch = do
            spaces
            _ <- char ch
            spaces

paddedStr :: String -> Parser ()
paddedStr str = do
            spaces
            _ <- string str
            spaces

parseSeq :: PiProcess -> Parser PiProcess
parseSeq p1 = do
            p2 <- try (do {paddedChar ';' ; parseProcess}) <|> return Null
            return $ p1 `Seq` p2

parseNew :: Parser PiProcess
parseNew = do
            _ <- string "new"
            spaces
            name <- parseTerm
            parseSeq $ New name

parseIf :: Parser PiProcess
parseIf = do
            _ <- string "if" 
            spaces
            cond <- parseCondition
            paddedStr "then"
            p1 <- parseProcess
            p2 <- try (do {paddedStr "else" ; parseProcess}) <|> return Null
            return $ If cond p1 p2

parseLet :: Parser PiProcess
parseLet = do
            _ <- string "let"
            spaces
            name <- parseTerm
            paddedChar '='
            val <- liftM Proc parseProcessNoAtom <|> liftM Term parseTerm
            p <- try (do 
                paddedStr "in"
                proc <- parseProcess
                return $ Just proc) <|> return Nothing
            return $ Let name val p
            where
                parseProcessNoAtom = bracketed parseProcess'' <|> parseProcess''
                parseProcess'' = parseNull 
                             <|> try parseIf
                             <|> try parseIn 
                             <|> try parseOut
                             <|> try parseLet
                             <|> parseReplicate
                             <|> parseNew

parseAtom :: Parser PiProcess
parseAtom = liftM Atom parseTerm

parseCondition :: Parser Condition
parseCondition = do
            t1 <- parseTerm
            paddedChar '='
            t2 <- parseTerm
            return $ t1 `Equals` t2

parseTVar :: Parser Term
parseTVar = do
        v <- readVar
        return $ case v of
            "true"  -> TBool True
            "false" -> TBool False
            _       -> TVar v

parseTFun :: Parser Term
parseTFun = do
            name <- readVar
            spaces
            args <- bracketed $ sepBy parseTerm paddedComma
            return $ case (name,args) of
                ("pair", t1:t2:_)  -> TPair  (t1,t2)
                _                  -> TFun name args (length args) 

parseTStr :: Parser Term
parseTStr = do
        _ <- char '"'
        x <- many $ noneOf "\""
        _ <- char '"'
        return $ TStr x

parseTNum :: Parser Term
parseTNum = liftM (TNum . read) (many1 digit)
        
readVar :: Parser Name
readVar = do
        frst <- letter <|> symbol
        rest <- many $ letter <|> digit <|> symbol
        return $ frst:rest

symbol :: Parser Char
symbol = oneOf "'._<>"

paddedComma :: Parser ()
paddedComma = paddedChar ','

parseTerm :: Parser Term
parseTerm =  try parseAnonChan
         <|> try parseTFun
         <|> parseTNum
         <|> parseTVar
         <|> parseTStr
         where
            parseAnonChan = do
                paddedChar '('
                arg <- many digit
                paddedChar ')'
                case arg of
                    [] -> return $ TFun "anonChan" [] 0
                    _  -> return $ TFun "anonChan" [TNum (read arg)] 1

parseProcess :: Parser PiProcess
parseProcess = liftM Conc $ sepBy parseProcess' (paddedChar '|')
    where
    parseProcess'  = bracketed parseProcess'' <|> parseProcess''
    parseProcess'' = parseNull 

                 <|> try parseIf
                 <|> try parseIn 
                 <|> try parseOut
                 <|> try parseLet
                 <|> parseAtom
                 <|> parseReplicate
                 <|> parseNew

bracketed :: Parser a -> Parser a
bracketed parser = do
                    _ <- char '('
                    spaces
                    res <- parser
                    spaces
                    _ <- char ')'
                    return res
