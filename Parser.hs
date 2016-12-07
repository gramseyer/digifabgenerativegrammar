module Parser(
    Program (PROGRAM),
    Definition (DEFINE),
    Statement(
        ST_MOVE,
        ST_STACKMANIP,
        ST_EMPTY,
        ST_APPLY,
        ST_COND,
        ST_ROTATEX,
        ST_ROTATEY, 
        ST_ROTATEZ,
        ST_ERASE,
        ST_DRAW,
        ST_FREEMOVE,
        ST_PERTURB),
    Param (
        PARAM_ID,
        PARAM_NUM),
    Expr (
        EXP_BINOP,
        EXP_VAR,
        EXP_NUM,
        EXP_FUNC),
    Identifier,
    parseProgram,
    Perturbation (
        P_HOLLOW,
        P_INVERT)) where

import Text.ParserCombinators.Parsec
import System.Environment
import Data.Map as Map
import Data.List as List

data Program = PROGRAM [Definition] Statement
                deriving Show

data Definition = DEFINE Identifier [Identifier] Statement
                  deriving Show

data Statement = ST_MOVE Expr Expr Expr Expr Statement
                | ST_STACKMANIP Statement Statement
                | ST_EMPTY
                | ST_APPLY Identifier [Expr] Statement
                | ST_COND Expr Statement Statement Statement
                | ST_ROTATEX Expr  Statement
                | ST_ROTATEY Expr Statement
                | ST_ROTATEZ Expr Statement
                | ST_ERASE Statement
                | ST_DRAW Statement
                | ST_FREEMOVE Statement
                | ST_PERTURB Perturbation Statement
                deriving Show

data Perturbation = P_HOLLOW Expr Expr Statement -- expr 1 ->  radius for draw expr 2 -> radius for erase 
                   | P_INVERT Statement -- turn all draw to erase and vice versa 
                   deriving Show

data Param = PARAM_ID Identifier
            | PARAM_NUM Float
            deriving Show

data Expr = EXP_BINOP Expr Expr (Float -> Float -> Float)
            | EXP_VAR Identifier
            | EXP_NUM Float
            | EXP_FUNC (Int, [Float] -> Float) [Expr] -- (arity, function) (expressions)

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
    show (EXP_FUNC (arity, _) vars) = "EXP_FUNC " ++ (show arity) ++ " " ++ (show vars)

type Identifier = String

-- Arity is validated later
sinFunc :: [Float] -> Float
sinFunc [x] = sin (pi*x/180)
sinFunc _ = -2

cosFunc :: [Float] -> Float
cosFunc [x] = cos (pi*x/180)
cosFunc _ = -2

sqrtFunc :: [Float] -> Float
sqrtFunc [x] = sqrt x
sqrtFunc _ = -1

basisFunctions :: Map Identifier (Int, [Float] -> Float)
basisFunctions = Map.fromList [("sin", (1, sinFunc)), ("cos", (1, cosFunc)), ("sqrt", (1, sqrtFunc))]

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
        <|> try stPerturb
        <|> stEmpty

stPerturb :: Parser Statement
stPerturb = try pHollow <|> pInvert 

pHollow :: Parser Statement 
pHollow = do
    string "HOLLOW"
    whiteSpace
    draw <- topExpr
    whiteSpace
    erase <- topExpr
    whiteSpace
    s <- statement 
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_PERTURB (P_HOLLOW draw erase s) remain

pInvert :: Parser Statement
pInvert = do
    string "INVERT"
    whiteSpace
    s <- statement
    char ';'
    whiteSpace
    remain <- statement
    return $ ST_PERTURB (P_INVERT s) remain

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
    params <- many topExpr 
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
    result <- (try topExpr <|> try funcExpr <|> varExpr <|> numExpr)
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

lookupFunc :: Identifier -> Parser (Int, [Float] -> Float)
lookupFunc funcName = case (Map.lookup funcName basisFunctions)
    of Just func -> return func
       Nothing -> fail $ "Function " ++ funcName ++ " not a valid basis function"

funcExpr :: Parser Expr
funcExpr = do
    funcName <- identifier
    params <- many1 (do whiteSpace
                        topExpr)
    whiteSpace
    function <- lookupFunc funcName
    return $ EXP_FUNC function params

floatParse :: Parser Float
floatParse = do
    up <- many digit
    char '.'
    remain <- many digit
    return $ read (up ++ ('.' : remain))

intParse :: Parser Float
intParse = do
    firstDigit <- digit
    temp <- many digit
    return $ read (firstDigit : temp)

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
