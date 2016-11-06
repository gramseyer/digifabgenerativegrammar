module Parser(Program (PROGRAM), Definition (DEFINE), Statement(ST_MOVE, ST_STACKMANIP, ST_EMPTY, ST_APPLY, ST_COND, ST_ROTATEX, ST_ROTATEY, ST_ROTATEZ, ST_ERASE, ST_DRAW, ST_FREEMOVE), Param (PARAM_ID, PARAM_NUM), Expr (EXP_BINOP, EXP_VAR, EXP_NUM), Identifier, parseProgram) where

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
                | ST_ROTATEX Expr Statement
                | ST_ROTATEY Expr Statement
                | ST_ROTATEZ Expr Statement
                | ST_ERASE Statement
                | ST_DRAW Statement
                | ST_FREEMOVE Statement
                deriving Show

data Param = PARAM_ID Identifier
            | PARAM_NUM Float
            deriving Show

data Expr = EXP_BINOP Expr Expr (Float -> Float -> Float)
            | EXP_VAR Identifier
            | EXP_NUM Float  

instance Show Expr where
    show (EXP_VAR identifier) = "EXP_VAR " ++ (show identifier)
    show (EXP_NUM number) = "EXP_NUM " ++ (show number)
    show (EXP_BINOP e1 e2 func) = case (func 3 3) of 
            6 -> "EXP_ADD " ++ (showBinop e1 e2)
            0 -> "EXP_SUB " ++ (showBinop e1 e2)
            9 -> "EXP_MUL " ++ (showBinop e1 e2)
            1 -> "EXP_DIV " ++ (showBinop e1 e2)
            _ -> "EXP_BINOP " ++ (showBinop e1 e2)
            where

showBinop e1 e2 = "(" ++ (show e1) ++ ") (" ++ (show e2) ++")"

type Identifier = String

parseProgram :: String -> Program
parseProgram str = case parse program "Invalid Parse" str of
    Left z -> error (show z) 
    Right x -> x

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
statement = try stMove 
        <|> try stStack
        <|> try stCond
        <|> try stApply
        <|> try stRotateX
        <|> try stRotateY
        <|> try stRotateZ
        <|> try stDraw
        <|> try stErase
        <|> try stFreeMove
        <|> stEmpty

stMove :: Parser Statement
stMove = do
    string "MOVE"
    whiteSpace
    x <- topExpr
    whiteSpace
    y <- topExpr
    whiteSpace
    z <- topExpr
    whiteSpace
    r <- topExpr
    whiteSpace
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
    rot <- topExpr
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_ROTATEX rot remain

stRotateY :: Parser Statement
stRotateY = do
    string "ROTATEY"
    whiteSpace
    rot <- topExpr
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_ROTATEY rot remain

stRotateZ :: Parser Statement
stRotateZ = do
    string "ROTATEZ"
    whiteSpace
    rot <- topExpr
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_ROTATEZ rot remain

stDraw :: Parser Statement
stDraw = do
    string "DRAW"
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_DRAW remain

stErase :: Parser Statement
stErase = do
    string "ERASE"
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_ERASE remain

stFreeMove :: Parser Statement
stFreeMove = do
    string "FREEMOVE"
    whiteSpace
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_FREEMOVE remain

stEmpty :: Parser Statement
stEmpty = return ST_EMPTY

identifier :: Parser Identifier
identifier = do
    first <- lower
    rest <- many alphaNum
    whiteSpace
    return $ (first : rest)

addSubExpr :: Parser Expr
addSubExpr = try addExpr <|> try subExpr <|> multDivExpr

addExpr :: Parser Expr
addExpr = do
    left <- multDivExpr
    whiteSpace
    char '+'
    whiteSpace
    remain <- addSubExpr
    return $ EXP_BINOP left remain (+)

subExpr :: Parser Expr
subExpr = do
    left <- multDivExpr
    whiteSpace
    char '-'
    whiteSpace
    remain <- addSubExpr
    return $ EXP_BINOP left remain (-)

multDivExpr :: Parser Expr
multDivExpr = try multExpr <|> try divExpr <|> terminalExpr

multExpr :: Parser Expr
multExpr = do
    left <- terminalExpr
    whiteSpace
    char '*'
    whiteSpace
    remain <- multDivExpr 
    return $ EXP_BINOP left remain (*)

divExpr :: Parser Expr
divExpr = do
    left <- terminalExpr
    whiteSpace
    char '/'
    whiteSpace
    remain <- multDivExpr 
    return $ EXP_BINOP left remain (/)


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
