module PiParser where

import Control.Monad (liftM)
import PiProcess
import Text.ParserCombinators.Parsec


parseIn :: Parser PiProcess
parseIn = do
            string "in("
            name <- readName
            paddedComma
            var  <- readVar
            char ')'
            return $ In name var 

parseOut :: Parser PiProcess
parseOut = do
            string "out("
            name <- readName
            paddedComma
            term  <- parseTerm
            char ')'
            return $ Out name term 

parseReplicate :: Parser PiProcess
parseReplicate = do
            string "!("
            process <- parseProcess
            char ')'
            return $ Replicate process

parseConc :: Parser PiProcess
parseConc = do
            p1 <- parseProcess 
            spaces
            char '|'
            spaces
            p2 <- parseProcess
            return $ p1 `Conc` p2

parseSeq :: Parser PiProcess
parseSeq = do
            p1 <- parseProcess 
            spaces
            char ';'
            spaces
            p2 <- parseProcess
            return $ p1 `Seq` p2

parseNew :: Parser PiProcess
parseNew = do
            string "new"
            spaces
            name <- readName
            return $ New name

parseIf :: Parser PiProcess
parseIf = do
            string "if" 
            spaces
            cond <- parseCondition
            spaces
            string "then"
            spaces
            p1 <- parseProcess
            spaces
            string "else"
            spaces
            p2 <- parseProcess
            return $ If cond p1 p2

parseLet :: Parser PiProcess
parseLet = do
            string "let"
            spaces
            name <- readName
            spaces
            char '='
            spaces
            term <- parseTerm
            spaces
            string "in"
            spaces
            p   <- parseProcess
            return $ Let name term p

parseCondition :: Parser Condition
parseCondition = do
            t1 <- parseTerm
            spaces
            string "=="
            spaces
            t2 <- parseTerm
            return $ t1 `Equals` t2

parseTName :: Parser Term
parseTName = liftM TName readName

parseTVar :: Parser Term
parseTVar = parseTName 

parseTFun :: Parser Term
parseTFun = do
            name <- readName
            spaces
            char '('
            spaces
            args <- sepBy parseTerm paddedComma
            spaces
            char ')'
            return $ TFun name args (length args)

readName :: Parser Name
readName = do
            first <- letter
            rest <- many $ letter <|> digit
            return $ first:rest

paddedComma :: Parser ()
paddedComma = do
                spaces
                char ','
                spaces

readVar :: Parser Variable
readVar = readName 

parseTerm :: Parser Term
parseTerm =  parseTName
         <|> parseTVar
         <|> parseTFun

parseProcess :: Parser PiProcess
parseProcess =  parseIn 
            <|> parseOut
            <|> parseNew
            <|> parseSeq
            <|> parseConc
            <|> parseReplicate
            <|> parseLet
            <|> parseIf

