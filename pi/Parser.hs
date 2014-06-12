module Parser (
        readTerm,
        readProcess,
        readProcesses)
        where

import Control.Monad (liftM)
import Control.Monad.Error (throwError)
import Text.ParserCombinators.Parsec

import TypDefs (Condition (..), PiProcess (..), Term (..), Type(..), Value(..), Name, PiError(Parser), ThrowsError)

parseNull :: Parser PiProcess
parseNull = char '0' >> return Null <?> "parse null"

parseIn :: Parser PiProcess
parseIn = do
            _ <- string "in("
            name <- parseTerm
            paddedComma
            var  <- parseTerm
            _ <- char ')'
            parseSeq $ In name var 
            <?> "parse in"

parseOut :: Parser PiProcess
parseOut = do
            _ <- string "out("
            name <- parseTerm
            paddedComma
            term  <- parseTerm
            _ <- char ')'
            parseSeq $ Out name term 
            <?> "parse out"

parseReplicate :: Parser PiProcess
parseReplicate = do
            _ <- string "!("
            process <- parseProcess
            _ <- char ')'
            return $ Replicate process
            <?> "parse replicate"

myOption :: Show b => a -> Parser a -> Parser b -> Parser a
myOption opt parser sep = try (notFollowedBy sep >> return opt) <|> (sep >> parser)

myOptionMaybe :: Show b => Parser a -> Parser b -> Parser (Maybe a)
myOptionMaybe parser = myOption Nothing (liftM Just parser)

parseSeq :: PiProcess -> Parser PiProcess
parseSeq p1 = do
            p2 <- myOption Null parseProcess (paddedChar ';')
            return $ p1 `Seq` p2

parseNew :: Parser PiProcess
parseNew = do
            _ <- string "new"
            spaces
            name <- parseTerm
            parseSeq $ New name
            <?> "parse new"


parseIf :: Parser PiProcess
parseIf = do
            _ <- string "if" 
            spaces
            cond <- parseCondition
            paddedStr "then"
            p1 <- parseProcess
            p2 <- myOption Null parseProcess (paddedStr1 "else")
            return $ If cond p1 p2 
            <?> "parse if"

parseLet :: Parser PiProcess
parseLet = do
            _ <- string "let"
            spaces
            name <- parseTerm
            paddedChar1 '='
            val <- try (liftM Proc parseProcess) <|> liftM Term parseTerm
            p <- myOptionMaybe parseProcess (paddedStr1 "in") 
            return $ Let name val p
            <?> "parse let"

parseAtom :: Parser PiProcess
parseAtom = do 
            _ <- char '&' 
            atom <- parseTerm 
            parseSeq $ Atom atom 

padded1 :: Parser a -> Parser ()
padded1 p = do 
         skipMany1 space
         _ <- p
         skipMany1 space

paddedChar :: Char ->  Parser ()
paddedChar ch = padded $ char ch

paddedStr :: String -> Parser ()
paddedStr str = padded $ string str

padded :: Parser a -> Parser ()
padded p = do 
         spaces
         _ <- p
         spaces

paddedChar1 :: Char ->  Parser ()
paddedChar1 ch = padded1 $ char ch

paddedStr1 :: String -> Parser ()
paddedStr1 str = padded1 $ string str

paddedComma :: Parser ()
paddedComma = paddedChar ','

parseCondition :: Parser Condition
parseCondition = do
            t1 <- parseTerm
            paddedChar1 '='
            t2 <- parseTerm
            return $ t1 `Equals` t2

parseTVar :: Parser Term
parseTVar = do
        v <- readVar
        case v of
            "true"  -> return $ TBool True
            "false" -> return $ TBool False
            _       -> do
                t <- myOptionMaybe parseType (paddedChar ':') 
                return $ TVar v t

parseTFun :: Parser Term
parseTFun = do
            name <- readVar
            spaces
            args <- bracketed $ sepBy parseTerm paddedComma
            return $ case (name,args) of
                ("pair", t1:t2:_)  -> TPair  (t1,t2)
                _                  -> TFun name args 

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

parseTerm :: Parser Term
parseTerm =  try parseAnonChan
         <|> try parseTFun
         <|> parseTNum
         <|> parseTVar
         <|> parseTStr
         where
            parseAnonChan = do
                _ <- char '{'
                spaces
                arg <- many parseTerm
                spaces
                _ <- char '}'
                return $ TFun "anonChan" arg

parseProcesses :: Parser [PiProcess]
parseProcesses = sepEndBy parseProcess newline

parseProcess :: Parser PiProcess
parseProcess = liftM (\ps -> case ps of 
                        [p] -> p
                        _   -> Conc ps) $ sepBy1 parseProcess' (char '|')
    where
    parseProcess'  = bracketed parseProcess'' <|> parseProcess''
    parseProcess'' = parseNull 
                 <|> try parseIf
                 <|> parseIn 
                 <|> parseOut
                 <|> parseLet
                 <|> parseReplicate
                 <|> parseNew
                 <|> parseAtom



parseType :: Parser Type
parseType =  try (str HttpRequest)
         <|> try (str HttpResponse)
         <|> try (str Header)
         <|> parseListType
         where
         str t = string (show t) >> return t
         parseListType = string "List" >> many1 space >> fmap List parseType 
        

bracketed :: Parser a -> Parser a
bracketed = between (char '(') (char ')')

readOrThrow :: Parser a -> String -> String -> ThrowsError a
readOrThrow parser name input = case parse parser name input of
                        Left  err -> throwError $ Parser err
                        Right val -> return val 

readProcess :: String -> ThrowsError PiProcess
readProcess = readOrThrow parseProcess "single process"

readProcesses :: String -> ThrowsError [PiProcess]
readProcesses = readOrThrow parseProcesses "multiple-processes"

readTerm :: String -> ThrowsError Term 
readTerm str = case parse parseTerm "term" str of
                Left  err -> throwError $ Parser err
                Right val -> return val 
