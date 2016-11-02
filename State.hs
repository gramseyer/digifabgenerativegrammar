module State (HeadState) where
import Parser
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import qualified Data.List as List

data HeadState = HEADSTATE Position Orientation Action

data Action = DRAW | ERASE | MOVE

-- There might be better libraries for these
data Orientation = ORIENT Vector Vector Vector

data Vector = VECTOR Float Float Float

data Position = LOC Vector Float -- x,y,z radius

-- State that persists through control flow
data PersistState = PERSIST HeadState [HeadState] [RecordedAction]

--Record of motion of the head through space.  
--Action, along with the discretized list of positions of the head
--as the motion was recorded.
data RecordedAction = RECORDED Action [Position]

-- State that is fixed at the start of evaluation, like function definitions
data FixedState = FIXED (Map.Map Parser.Identifier ([Parser.Identifier], Parser.Statement))

-- State that doesn't persist after the end of a statement - like function parameter bindings
-- Variable bindings are only introduced in expr evaluation (t) and function calls
data EphemeralState = EPHEMERAL [Map.Map Parser.Identifier Float]

data TotalState = TOTAL FixedState EphemeralState PersistState

type Model a = ExceptT String (State TotalState) a --StateT s (Either String) TotalState

{-evalMotion :: Parser.Expr -> Parser.Expr -> Parser.Expr -> Parser.Expr -> State TotalState
evalMotion x y z r

evalStatement :: Parser.Statement -> State TotalState
evalStatement (Parser.ST_MOVE x y z r stm) = do
    evalMotion x y z r
-}

getHeadState :: Model HeadState
getHeadState = state $ \(TOTAL f e (PERSIST h stk records)) ->  (h,(TOTAL f e (PERSIST h stk records)))

pushHeadToStack :: Model ()
pushHeadToStack = state $ \(TOTAL f e (PERSIST h stk records)) -> ((), TOTAL f e (PERSIST h (h:stk) records))

popHeadFromStack :: Model HeadState
popHeadFromStack = state $ \(TOTAL f e (PERSIST old (h:stk) records)) -> (old, TOTAL f e (PERSIST h stk records))

getVarBindings :: Model (Map.Map Parser.Identifier Float)
getVarBindings = state $ \(TOTAL f (EPHEMERAL (m:m')) p) -> (m, TOTAL f (EPHEMERAL (m:m')) p)

pushVarBindings :: [(Parser.Identifier, Float)] -> Model ()
pushVarBindings bindings = state $ \(TOTAL f (EPHEMERAL (oldBindings : stk)) p) ->
                                ((), TOTAL f (EPHEMERAL ((List.foldr addBinding oldBindings bindings) : (oldBindings : stk))) p)

popVarBindings :: Model (Map.Map Parser.Identifier Float)
popVarBindings = state $ \(TOTAL f (EPHEMERAL (m:m')) p) -> (m, TOTAL f (EPHEMERAL m') p)

addBinding :: (Parser.Identifier, Float) -> Map.Map Parser.Identifier Float -> Map.Map Parser.Identifier Float
addBinding (key, value) bindings = Map.insert key value bindings

--example of how to throw an error
errTest :: String -> Model ()
errTest str = throwError str

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