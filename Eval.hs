module Eval (executeProgram) where
import Parser
import State
import qualified Data.Map as Map
import Control.Monad.Except
import qualified Data.List as List

executeProgram :: Parser.Program -> Model ()
executeProgram (Parser.PROGRAM definitions statement) = (addDefinitions definitions) >> (executeStatement statement)

executeStatement :: Parser.Statement -> Model ()
executeStatement statement = throwError "Unimplemented"

executeCond :: Parser.Expr -> Parser.Statement -> Parser.Statement -> Parser.Statement -> Model ()
executeCond expr branch1 branch2 rest = do
    cond <- evalInternal expr 
    if (cond >= 0)
        then executeStatement branch1
        else executeStatement branch2
    executeStatement rest

executeEmpty :: Parser.Statement -> Model ()
executeEmpty statement = do
    return ()

executeStackManip :: Parser.Statement -> Parser.Statement -> Model ()
executeStackManip statement rest = do
    pushHeadToStack
    executeStatement statement
    popHeadFromStack
    executeStatement rest

executeApply :: Parser.Identifier -> [Parser.Param] -> Parser.Statement -> Model ()
executeApply funcName funcArgs rest = do
    bindings <- getVarBindings
    (argNames, statement) <- findDefinition funcName
    let params = List.map (loadParam bindings) funcArgs
    validateParams funcName argNames params
    pushVarBindings (List.zip argNames (List.map unJust params))
    executeStatement statement
    popVarBindings
    executeStatement rest
    where
        unJust (Just x) = x
        unJust Nothing = error "This should have been checked earlier"

loadParam :: Map.Map Parser.Identifier Float -> Parser.Param -> Maybe Float
loadParam bindings (PARAM_NUM num) = Just num
loadParam bindings (PARAM_ID identifier) = Map.lookup identifier bindings

validateParams :: Parser.Identifier -> [Parser.Identifier] -> [Maybe Float] -> Model ()
validateParams funcName (iden:idens) (param:params) = case param of
    Just _ -> validateParams funcName idens params
    Nothing -> throwError $ "unresolved input: Argument " ++ iden ++ " of function " ++ funcName
validateParams _ [] [] = do
    return ()
validateParams funcName _ _ = throwError $ "mismatched number of args for function " ++ funcName


timeVar :: Parser.Identifier
timeVar = "t"

evalTimeExpr :: Parser.Expr -> Float -> Model Float
evalTimeExpr expr time = do
    pushVarBindings [(timeVar, time)]
    result <- evalInternal expr
    popVarBindings
    return result

evalInternal :: Parser.Expr -> Model Float
evalInternal (Parser.EXP_BINOP e1 e2 func) = liftBinop func (evalInternal e1) (evalInternal e2)
evalInternal (Parser.EXP_NUM number) = do
    return number
evalInternal (Parser.EXP_VAR var) = do
    bindings <- getVarBindings
    (unpackVar var (Map.lookup var bindings))

liftBinop :: (Float -> Float -> Float) -> Model Float -> Model Float -> Model Float
liftBinop binop evalExpr1 evalExpr2 = do
    result1 <- evalExpr1
    result2 <- evalExpr2
    return $ result1 `binop` result2

unpackVar :: Parser.Identifier -> Maybe Float -> Model Float
unpackVar var Nothing = throwError ("Variable " ++ var ++ " not found")
unpackVar _ (Just result) = do
    return result

