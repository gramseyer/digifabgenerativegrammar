module Parser(Program, Definition, Statement, Param, Expr, Identifier, parseProgram) where

import Text.ParserCombinators.Parsec
import System.Environment

data Program = PROGRAM [Definition] Statement
               deriving Show

data Definition = DEFINE Identifier [Identifier] Statement
                  deriving Show

data Statement = ST_MOVE Expr Expr Expr Expr Statement
                | ST_STACKMANIP Statement Statement
                | ST_EMPTY
                | ST_APPLY Identifier [Param] Statement
                | ST_COND Expr Statement Statement Statement
                | ST_ROTATEX Float Statement
                | ST_ROTATEY Float Statement
                | ST_ROTATEZ Float Statement
                deriving Show

data Param = PARAM_ID Identifier
            | PARAM_NUM Float
            deriving Show

data Expr = EXP_ADD Expr Expr
            | EXP_SUB Expr Expr
            | EXP_MULT Expr Expr
            | EXP_DIV Expr Expr
            | EXP_VAR Identifier
            | EXP_NUM Float  
            deriving Show   

data Identifier = ID String deriving Show

parseProgram :: String -> Either ParseError Program
parseProgram = parse program "Invalid Parse"

program :: Parser Program
program = do
    definitions <- many definition
    action <- statement
    eof
    return $ PROGRAM definitions action

definition :: Parser Definition
definition = do
    char '('
    whiteSpace
    string "DEFINE"
    whiteSpace
    iden <- identifier
    whiteSpace
    char '('
    whiteSpace
    vars <- many identifier
    whiteSpace
    char ')'
    whiteSpace
    action <- statement
    char ')'
    whiteSpace
    return $ DEFINE iden vars action

param :: Parser Param
param = try paramID <|> paramNum 

paramID :: Parser Param 
paramID = do
    test <- identifier
    whiteSpace
    return $ PARAM_ID test

paramNum :: Parser Param 
paramNum = do
    test <- numParse
    whiteSpace
    return $ PARAM_NUM test

statement :: Parser Statement 
statement = try stMove <|> try stStack <|> try stCond <|> try stApply <|> try stRotateX <|> try stRotateY <|> try stRotateZ <|> stEmpty

stMove :: Parser Statement
stMove = do
    string "MOVE"
    whiteSpace
    x <- topExpr
    y <- topExpr
    z <- topExpr
    r <- topExpr
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_MOVE x y z r remain

stStack :: Parser Statement
stStack = do
    char '['
    whiteSpace
    states <- statement 
    char ']'
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_STACKMANIP states remain

stCond :: Parser Statement
stCond = do
    string "IF"
    whiteSpace 
    bool <- topExpr
    whiteSpace
    string "THEN"
    whiteSpace
    true <- statement
    whiteSpace
    string "ELSE"
    whiteSpace
    false <- statement
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_COND bool true false remain

stApply :: Parser Statement
stApply = do
    char '{'
    whiteSpace
    iden <- identifier
    whiteSpace
    params <- many param 
    char '}'
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_APPLY iden params remain

stRotateX :: Parser Statement
stRotateX = do
    string "ROTATEX"
    whiteSpace
    rot <- numParse
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_ROTATEX rot remain

stRotateY :: Parser Statement
stRotateY = do
    string "ROTATEY"
    whiteSpace
    rot <- numParse
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_ROTATEY rot remain

stRotateZ :: Parser Statement
stRotateZ = do
    string "ROTATEZ"
    whiteSpace
    rot <- numParse
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_ROTATEZ rot remain

stEmpty :: Parser Statement
stEmpty = return ST_EMPTY

identifier :: Parser Identifier
identifier = do
    first <- lower
    rest <- many alphaNum
    whiteSpace
    return $ ID (first : rest)

addSubExpr :: Parser Expr
addSubExpr = try addExpr <|> try subExpr <|> multDivExpr

addExpr :: Parser Expr
addExpr = do
    left <- multDivExpr
    whiteSpace
    char '+'
    whiteSpace
    remain <- addSubExpr
    return $ EXP_ADD left remain

subExpr :: Parser Expr
subExpr = do
    left <- multDivExpr
    whiteSpace
    char '-'
    whiteSpace
    remain <- addSubExpr
    return $ EXP_SUB left remain

multDivExpr :: Parser Expr
multDivExpr = try multExpr <|> try divExpr <|> terminalExpr

multExpr :: Parser Expr
multExpr = do
    left <- terminalExpr
    whiteSpace
    char '*'
    whiteSpace
    remain <- multDivExpr 
    return $ EXP_MULT left remain

divExpr :: Parser Expr
divExpr = do
    left <- terminalExpr
    whiteSpace
    char '/'
    whiteSpace
    remain <- multDivExpr 
    return $ EXP_DIV left remain


terminalExpr :: Parser Expr
terminalExpr = do
    result <- (try topExpr <|> try varExpr <|> numExpr)
    whiteSpace
    return result

topExpr :: Parser Expr
topExpr = do
    char '('
    temp <- addSubExpr
    char ')'
    return temp

varExpr :: Parser Expr
varExpr = do
    test <- identifier 
    return $ EXP_VAR test

numExpr :: Parser Expr
numExpr = try negExpr <|> posExpr

floatParse :: Parser Float
floatParse = do
    up <- many digit
    char '.'
    remain <- many digit
    return $ read (up ++ ('.' : remain))

intParse :: Parser Float
intParse = do
    temp <- many digit
    return $ read temp

numParse :: Parser Float
numParse = try floatParse <|> intParse

posExpr :: Parser Expr
posExpr = do
    test <- numParse
    return $ EXP_NUM test

negExpr :: Parser Expr
negExpr = do
    char '~'
    test <- numParse
    return $ EXP_NUM (-test)

whiteSpace :: Parser ()
whiteSpace = do
    many (satisfy isWhiteSpace)
    return ()
    

isWhiteSpace :: Char -> Bool
isWhiteSpace ' ' = True
isWhiteSpace '\n' = True
isWhiteSpace '\t' = True
isWhiteSpace _ = False
