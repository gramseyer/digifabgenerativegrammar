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

program :: Parser Program
program = do
    definitions <- many definition
    action <- statement
    return $ PROGRAM definitions action

definition :: Parser Definition
definition = do
    char '('
    string "DEFINE"
    iden <- identifier
    char '('
    vars <- many identifier
    char ')'
    action <- statement
    char ')'
    return $ DEFINE iden vars action

param :: Parser Param
param = try paramID <|> paramNum 

paramID :: Parser Param 
paramID = do
    test <- identifier
    return $ PARAM_ID test

paramNum :: Parser Param 
paramNum = do
    test <- numParse
    return $ PARAM_NUM test

statement :: Parser Statement 
statement = try stMove <|> try stStack <|> try stCond <|> try stApply <|> stEmpty

stMove :: Parser Statement
stMove = do
    string "MOVE"
    x <- topExpr
    y <- topExpr
    z <- topExpr
    r <- topExpr
    char ';'
    remain <- statement
    return $ ST_MOVE x y z r remain

stStack :: Parser Statement
stStack = do
    char '['
    states <- statement 
    char ']'
    char ';'
    remain <- statement
    return $ ST_STACKMANIP states remain

stCond :: Parser Statement
stCond = do
    string "IF" 
    bool <- topExpr
    string "THEN"
    true <- statement
    string "ELSE"
    false <- statement
    char ';'
    remain <- statement
    return $ ST_COND bool true false remain

stApply :: Parser Statement
stApply = do
    char '}'
    iden <- identifier
    params <- many param 
    char '}'
    char ';'
    remain <- statement
    return $ ST_APPLY iden params remain

stEmpty :: Parser Statement
stEmpty = return ST_EMPTY

identifier :: Parser Identifier
identifier = do
    first <- lower
    rest <- many alphaNum
    return $ ID (first : rest)

addSubExpr :: Parser Expr
addSubExpr = try addExpr <|> try subExpr <|> multDivExpr

addExpr :: Parser Expr
addExpr = do
    left <- multDivExpr
    char '+'
    remain <- addSubExpr
    return $ EXP_ADD left remain

subExpr :: Parser Expr
subExpr = do
    left <- multDivExpr
    char '-'
    remain <- addSubExpr
    return $ EXP_SUB left remain

multDivExpr :: Parser Expr
multDivExpr = try multExpr <|> try divExpr <|> terminalExpr

multExpr :: Parser Expr
multExpr = do
    left <- terminalExpr
    char '*'
    remain <- multDivExpr 
    return $ EXP_MULT left remain

divExpr :: Parser Expr
divExpr = do
    left <- terminalExpr
    char '/'
    remain <- multDivExpr 
    return $ EXP_DIV left remain


terminalExpr :: Parser Expr
terminalExpr = try topExpr <|> try varExpr <|> numExpr

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