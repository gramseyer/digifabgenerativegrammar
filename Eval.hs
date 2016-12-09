module Eval (executeProgram) where
import Parser
import State
import qualified Data.Map as Map
import Control.Monad.Except
import qualified Data.List as List

executeProgram :: Parser.Program -> Model ()
executeProgram (Parser.PROGRAM definitions statement) =
        (addDefinitions definitions) >> (executeStatement statement)

executeStatement :: Parser.Statement -> Model ()
executeStatement statement = case statement of
    (Parser.ST_MOVE x y z r st)             -> executeMove x y z r st
    (Parser.ST_STACKMANIP st st')           -> executeStackManip st st'
    (Parser.ST_EMPTY)                       -> executeEmpty 
    (Parser.ST_APPLY identifier params st)  -> executeApply identifier params st
    (Parser.ST_COND expr st1 st2 st')       -> executeCond expr st1 st2 st'
    (Parser.ST_ROTATEX expr st)             -> executeRotateX expr st
    (Parser.ST_ROTATEY expr st)             -> executeRotateY expr st
    (Parser.ST_ROTATEZ expr st)             -> executeRotateZ expr st
    (Parser.ST_ERASE st)                    -> setErase >> (executeStatement st)
    (Parser.ST_FREEMOVE st)                 -> setMove >> (executeStatement st)
    (Parser.ST_DRAW st)                     -> setDraw >> (executeStatement st)
    (Parser.ST_PERTURB p st)                -> executePerturb p st

executePerturb :: Parser.Perturbation -> Parser.Statement -> Model ()
executePerturb p remain =  case p of
    (Parser.P_INVERT st)            -> executeInvert st >> executeStatement remain
    (Parser.P_HOLLOW draw erase st) -> executeHollow draw erase st  >> executeStatement remain

executeHollow :: Parser.Expr -> Parser.Expr -> Parser.Statement -> Model()
executeHollow drawExpr eraseExpr st = runSubmodel ((executeStatement st) >> getRecords >>= (hollowRecords drawExpr eraseExpr))

hollowRecords :: Parser.Expr -> Parser.Expr -> [RecordedActions] -> Model ()
hollowRecords _ _ [] = do (return ())
hollowRecords draw erase records = List.foldr1 (>>) (List.map (hollowRecord draw erase) records)

mergeFunc :: ([a] -> Model ([a])) -> ([a] -> Model ([a])) -> ([a] -> Model ([a]))
mergeFunc f g = \values -> (f values) >>= g

hollowRecord :: Parser.Expr -> Parser.Expr -> RecordedActions -> Model ()
hollowRecord _ _ (_, []) = do (return ())
hollowRecord draw erase (MOVE, positions) = do (return ())
hollowRecord draw erase (DRAW, positions) = 
    ((List.foldr1 mergeFunc (List.map (\pos -> hollowPoint draw pos) positions)) [])
    >>= (\positions -> addRecords [(ERASE, positions)])
hollowRecord draw erase (ERASE, positions) = 
    ((List.foldr1 mergeFunc (List.map (\pos -> hollowPoint erase pos) positions)) [])
    >>= (\positions -> addRecords [(DRAW, positions)])

hollowPoint :: Parser.Expr -> Position -> [Position] -> Model ([Position])
hollowPoint expr (loc, r) processed = do
    pushVarBindings [("r", r)] 
    newRadius <- evalInternal expr
    popVarBindings
    return $ ((loc, newRadius):processed)

executeInvert :: Parser.Statement -> Model ()
executeInvert st = runSubmodel ((executeStatement st)>> 
    (do
        records <- getRecords
        clearRecords
        addRecords $ invertRecords records))

invertRecords :: [RecordedActions] -> [RecordedActions]
invertRecords = List.map invert

invert :: RecordedActions -> RecordedActions
invert (DRAW, pos) = (ERASE, pos)
invert (ERASE, pos) = (DRAW, pos)
invert (MOVE, pos) = (MOVE, pos)

executeMove :: Parser.Expr -> Parser.Expr -> Parser.Expr -> Parser.Expr -> Parser.Statement -> Model ()
executeMove xExpr yExpr zExpr rExpr statement = do
    poses <- generatePositions xExpr yExpr zExpr rExpr
    moveDiscretised poses
    executeStatement statement

executeStackManip :: Parser.Statement -> Parser.Statement -> Model ()
executeStackManip statement rest = do
    pushHeadToStack
    executeStatement statement
    popHeadFromStack
    executeStatement rest

executeCond :: Parser.Expr -> Parser.Statement -> Parser.Statement -> Parser.Statement -> Model ()
executeCond expr branch1 branch2 rest = do
    cond <- evalInternal expr 
    if (cond >= 0)
        then executeStatement branch1
        else executeStatement branch2
    executeStatement rest

executeEmpty :: Model ()
executeEmpty = do
    return ()

makeParamBindings :: [Parser.Expr] -> [Parser.Identifier] -> Model ([(Parser.Identifier, Float)])
makeParamBindings (expr : exprs) (iden : idens) = do
    evaluated <- evalInternal expr
    rest <- makeParamBindings exprs idens
    return $ (iden, evaluated) : rest
makeParamBindings [] [] = do
    return []
makeParamBindings _ _ = error "Non matching number of arguments/parameters."

executeApply :: Parser.Identifier -> [Parser.Expr] -> Parser.Statement -> Model ()
executeApply funcName funcArgs rest = do
    bindings <- getVarBindings
    (argNames, statement) <- findDefinition funcName
    bindings <- makeParamBindings funcArgs argNames
    pushVarBindings bindings
    executeStatement statement
    popVarBindings
    executeStatement rest
    where
        unJust (Just x) = x
        unJust Nothing = error "This should have been checked earlier"

executeRotateX :: Parser.Expr -> Parser.Statement -> Model ()
executeRotateX expr statement = do
    theta <- evalInternal expr
    rotateX theta
    executeStatement statement

executeRotateY :: Parser.Expr -> Parser.Statement -> Model ()
executeRotateY expr statement = do
    theta <- evalInternal expr
    rotateY theta
    executeStatement statement

executeRotateZ :: Parser.Expr -> Parser.Statement -> Model ()
executeRotateZ expr statement = do
    theta <- evalInternal expr
    rotateZ theta
    executeStatement statement

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

consAccumulate :: Model x -> Model [x] -> Model [x]
consAccumulate model model' = do
    x <- model
    xs <- model'
    return $ x:xs

baseAccumulate :: Model [x]
baseAccumulate = do
    return []

generatePositions :: Parser.Expr -> Parser.Expr -> Parser.Expr -> Parser.Expr -> Model [State.Position]
generatePositions x y z r = List.foldr consAccumulate (baseAccumulate) (mergedPositions)
    where
        positions = List.zip4 (evalTimeExprLifted x timeRange)
                              (evalTimeExprLifted y timeRange)
                              (evalTimeExprLifted z timeRange)
                              (evalTimeExprLifted r timeRange)
        mergedPositions = List.map mergeModels positions

mergeModels :: (Model Float, Model Float, Model Float, Model Float) -> Model State.Position
mergeModels (xM, yM, zM, rM) = do
    x<-xM
    y<-yM
    z<-zM
    r<-rM
    return ((x,y,z),r)

timeRange :: [Float]
timeRange = [0,0.05..1]

timeVar :: Parser.Identifier
timeVar = "t"

evalTimeExprLifted :: Parser.Expr -> [Float] -> [Model Float]
evalTimeExprLifted expr = liftM (evalTimeExpr expr)

evalTimeExpr :: Parser.Expr -> Float -> Model Float
evalTimeExpr expr time = do
    pushVarBindings [(timeVar, time)]
    result <- evalInternal expr
    popVarBindings
    return result

accumulateFloats :: Parser.Expr -> [Float] -> Model [Float]
accumulateFloats expr prev = do
    result <- evalInternal expr
    return $ prev ++ [result]

evalArgs :: [Parser.Expr] -> Model ([Float])
evalArgs exprs = ((List.foldr1 mergeFunc (List.map (\expr -> accumulateFloats expr) exprs)) [])

evalInternal :: Parser.Expr -> Model Float
evalInternal (Parser.EXP_BINOP e1 e2 func) = liftBinop func (evalInternal e1) (evalInternal e2)
evalInternal (Parser.EXP_NUM number) = do
    return number
evalInternal (Parser.EXP_VAR var) = do
    bindings <- getVarBindings
    (unpackVar var (Map.lookup var bindings))
evalInternal (Parser.EXP_FUNC (arity, func) args) = if (arity /= List.length args) then
        throwError ("Mismatched function arity: " ++ show arity ++ " /= " ++ (show $ List.length args))
    else
        do
            inputs <- evalArgs args
            return $ func inputs


liftBinop :: (Float -> Float -> Float) -> Model Float -> Model Float -> Model Float
liftBinop binop evalExpr1 evalExpr2 = do
    result1 <- evalExpr1
    result2 <- evalExpr2
    return $ result1 `binop` result2

unpackVar :: Parser.Identifier -> Maybe Float -> Model Float
unpackVar var Nothing = throwError ("Variable " ++ var ++ " not found")
unpackVar _ (Just result) = do
    return result

