module Main where

import Control.Monad (liftM)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec

data PiProcess = Null
               | In   Name Variable
               | Out  Name Term
               | New  Name
               | PiProcess `Seq`   PiProcess -- Sequential Composition
               | PiProcess `Conc`  PiProcess -- Parallel   Composition
               | Replicate PiProcess         -- Infinite parallel replication
               | Let Name Term PiProcess
               | If Condition PiProcess PiProcess
                 deriving (Eq, Show)

data Term = TVar Variable
          | TFun Name [Term] Int
            deriving (Eq, Show)

type Variable   = String
type Name      = String
data Condition = Term `Equals` Term deriving (Eq, Show)

showPi :: PiProcess -> String
showPi Null = "0"
showPi (In c m) =  "in(" ++ c ++ "," ++ m ++ ")"
showPi (Out c m) =  "out(" ++ c ++ "," ++  show m ++ ")"
showPi (Replicate proc) =  "!(" ++ show proc ++ ")"
showPi (p1 `Conc` p2) = show p1 ++ "|\n" ++ show p2
showPi (p1 `Seq` Null) = show p1
showPi (p1 `Seq` p2) = show p1 ++ ";\n" ++ show p2 
showPi (New n)   = "new " ++ n
showPi (If c p1 p2) = "if " ++ show c ++ " then " ++ show p1 ++ " else " ++ show p2
showPi (Let n t p) = "let " ++ n ++ " = " ++ show t ++ " in\n" ++ show p

showTerm :: Term -> String
showTerm (TVar x) = x
showTerm (TFun n [] 0) = n ++ "()"
showTerm (TFun n ts _) = n ++ "(" ++ intercalate "," (map show ts) ++ ")"

showCond :: Condition -> String
showCond (t1 `Equals` t2) = show t1 ++ " == " ++ show t2

-- instance Show PiProcess where show = showPi
-- instance Show Term where show = showTerm
-- instance Show Condition where show = showCond

parseNull :: Parser PiProcess
parseNull = do
            char '0'
            return Null

parseIn :: Parser PiProcess
parseIn = do
            string "in("
            name <- readVar
            paddedComma
            var  <- readVar
            char ')'
            parseSeq $ In name var 

parseOut :: Parser PiProcess
parseOut = do
            string "out("
            name <- readVar
            paddedComma
            term  <- parseTerm
            char ')'
            parseSeq $ Out name term 

parseReplicate :: Parser PiProcess
parseReplicate = do
            string "!("
            process <- parseProcess
            char ')'
            return $ Replicate process

paddedChar :: Char ->  Parser ()
paddedChar ch= do
            spaces
            char ch
            spaces

parseSeq :: PiProcess -> Parser PiProcess
parseSeq p1 = do
            paddedChar ';'
            p2 <- parseProcess
            return $ p1 `Seq` p2

parseNew :: Parser PiProcess
parseNew = do
            string "new"
            spaces
            name <- readVar
            parseSeq $ New name

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
            name <- readVar
            paddedChar '='
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

parseTVar :: Parser Term
parseTVar = liftM TVar readVar

parseTFun :: Parser Term
parseTFun = do
            name <- readVar
            spaces
            args <- bracketed $ sepBy parseTerm paddedComma
            return $ TFun name args (length args)

readVar :: Parser Name
readVar = do
            frst <- letter
            rest <- many $ letter <|> digit
            return $ frst:rest

paddedComma :: Parser ()
paddedComma = paddedChar ','


parseTerm :: Parser Term
parseTerm =  try parseTFun
         <|> parseTVar

parseProcess :: Parser PiProcess
parseProcess = liftM (foldr1 Conc) $ sepBy parseProcess' (paddedChar '|')
    where
    parseProcess'  = bracketed parseProcess'' <|> parseProcess''
    parseProcess'' = parseNull 
                <|> parseIn 
                <|> parseOut
                <|> parseReplicate
                <|> parseNew
                <|> parseLet
                <|> parseIf

bracketed :: Parser a -> Parser a
bracketed parser = do
                    char '('
                    spaces
                    res <- parser
                    spaces
                    char ')'
                    return res

type TermFun = [Term] -> Term
primitives :: [(String      , TermFun)]
primitives = [ ("true"      , constId "true")
             , ("false"     , constId "false")
             , ("fst"       , first)
             , ("snd"       , second)
             , ("hash"      , unaryId "hash")
             , ("pk"        , unaryId "pk")
             , ("getmsg"    , getmsg)
             , ("pair"      , binaryId "pair")
             , ("sdec"      , sdec)
             , ("senc"      , binaryId "senc")
             , ("adec"      , adec)
             , ("aenc"      , binaryId "aenc")
             , ("sign"      , binaryId "sign")
             , ("checksign" , binaryId "checksign")
             , ("mac"       , binaryId "mac")
             ]

constId :: String -> TermFun
constId name [] = TFun name [] 0
constId name _  = error $ "incorrect parity for " ++ name

unaryId :: String -> TermFun
unaryId name [x] = TFun name [x] 1
unaryId name _  = error $ "incorrect parity for " ++ name

binaryId :: String ->  TermFun
binaryId name [x,y] = TFun name [x,y] 2
binaryId name _     = error $ "incorrect parity for " ++ name

getmsg :: TermFun
getmsg [TFun "sign" [_,y] 2] = y
getmsg l@_ = error $  "getmsg expected sign(x,y) got: " ++ show l

first :: TermFun
first [TFun "pair" [x, _] 2] = x
first _  = error "fst not given pair"

second :: TermFun
second [TFun "pair" [_,y] 2] = y
second _ = error "second not given pair"

sdec :: TermFun
sdec [k1, TFun "senc" [k2,y] 2]
    |k1 == k2  = y
    |otherwise = error "keys not the same in sdec"
sdec _ = error "sdec not given pair"

adec :: TermFun
adec [x , TFun "aenc" [TFun "pk" [k] 1, y ] 2 ]
    | x == k = y
    | otherwise= error $ "keys not same in adec" 
adec e@_ = error $ "checksign expected (x,aenc(pk(x),y)), got: " ++ show e

checksign :: TermFun
checksign [TFun "pk" [k1] 1, TFun "sign" [k2,_] 2 ]
    | k1 == k2 = constId "true" [] 
    | otherwise= error $ "keys not same in checksign" 
checksign e@_ = error $ "checksign expected (pk(x),sign(x,y)), got: " ++ show e

main :: IO ()
main = do
        args <- getArgs 
        let f = case args of
                    []  -> readFile "test.pi" 
                    [x] -> readFile x
        progs <- liftM lines f
        putStrLn $ intercalate  "\n\n" $ map readProgram progs
        putStrLn ""

readProgram :: String ->  String
readProgram input = case parse parseProcess "pi-calculus" input of
                        Left  err -> show err
                        Right val -> show val 

evalTerm :: Term -> Term
evalTerm val@(TVar _) = val
evalTerm (TFun name args _) = fun $ map evalTerm args
        where 
            fun = fromJust $ lookup name primitives

eval :: PiProcess -> IO ()
eval Null = undefined
eval (In c m) = undefined
eval (Out c m) = undefined
eval (Replicate proc)= undefined
eval (p1 `Conc` p2) = undefined
eval (p1 `Seq` p2) = undefined 
eval (New n)   = undefined 
eval (If c p1 p2) = undefined 
eval (Let n t p) = undefined 
