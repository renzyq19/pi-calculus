module PiParser where

import PiProcess
import Text.ParserCombinators.Parsec


parseIn :: Parser PiProcess
parseIn = do
           string "in("
           name <- readName
           char ','
           var  <- readVar
           char ')'
           return $ In name var 

parseOut :: Parser PiProcess
parseOut = do
           string "out("
           name <- readName
           char ','
           term  <- readTerm
           char ')'
           return $ Out name term 

parseProcess :: Parser PiProcess
parseProcess = undefined
parseReplicate :: Parser PiProcess
parseReplicate = do
            string "!("
            process <- 


