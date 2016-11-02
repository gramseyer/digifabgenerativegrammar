
import Parser
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

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
data EphemeralState = EPHEMERAL (Map.Map Parser.Identifier Float)

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
popHeadFromStack = state $ \(TOTAL f e (PERSIST _ (h:stk) records)) -> (h, TOTAL f e (PERSIST h stk records))