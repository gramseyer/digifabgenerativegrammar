import Parser
import State
import qualified Data.Map as Map
import Control.Monad.Except

timeVar :: Parser.Identifier
timeVar = "t"

evalExpr :: Parser.Expr -> Float -> Model Float
evalExpr expr time = do
    pushVarBindings [(timeVar, time)]
    result <- evalInternal expr
    popVarBindings
    return result

liftBinop :: (Float -> Float -> Float) -> Model Float -> Model Float -> Model Float
liftBinop binop evalExpr1 evalExpr2 = do
    result1 <- evalExpr1
    result2 <- evalExpr2
    return $ result1 `binop` result2

evalInternal :: Parser.Expr -> Model Float
evalInternal (Parser.EXP_BINOP e1 e2 func) = liftBinop func (evalInternal e1) (evalInternal e2)
evalInternal (Parser.EXP_NUM number) = do
    return number
evalInternal (Parser.EXP_VAR var) = do
    bindings <- getVarBindings
    (unpackVar var (Map.lookup var bindings))

unpackVar :: Parser.Identifier -> Maybe Float -> Model Float
unpackVar var Nothing = throwError ("Variable " ++ var ++ " not found")
unpackVar _ (Just result) = do
    return result