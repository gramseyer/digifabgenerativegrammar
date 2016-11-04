module State (Model, runModel, rotateX, rotateY, rotateZ, initialState, getHeadState, pushHeadToStack, popHeadFromStack, getVarBindings, pushVarBindings, popVarBindings, addDefinitions, findDefinition) where
import Parser
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import qualified Data.List as List

data HeadState = HEADSTATE Position Orientation Action

data Action = DRAW | ERASE | MOVE

-- There might be better libraries for these
data Orientation = ORIENT Vector Vector Vector

data Vector = VECTOR Float Float Float deriving Show

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

runModel :: Model a -> (Either String a, TotalState)
runModel model = runState (runExceptT model) initialState

initialHeadState :: HeadState
initialHeadState = HEADSTATE (LOC (VECTOR 0 0 0) 0) (ORIENT (VECTOR 1 0 0) (VECTOR 0 1 0) (VECTOR 0 0 1)) DRAW

initialState :: TotalState
initialState = TOTAL (FIXED Map.empty) (EPHEMERAL [Map.empty]) (PERSIST initialHeadState [] [])

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

addDefinitions :: [Parser.Definition] -> Model ()
addDefinitions funcs = state $ \(TOTAL (FIXED prevMap) e p) -> 
                            ((), TOTAL (FIXED (List.foldr addDefinition prevMap funcs)) e p)

addDefinition :: Parser.Definition -> Map.Map Parser.Identifier ([Parser.Identifier], Parser.Statement) -> Map.Map Parser.Identifier ([Parser.Identifier], Parser.Statement)
addDefinition (DEFINE name args statement) defns = Map.insert name (args, statement) defns

getDefinitions :: Model (Map.Map Parser.Identifier ([Parser.Identifier], Parser.Statement))
getDefinitions = state $ \(TOTAL (FIXED prevMap) e p) -> (prevMap, TOTAL (FIXED prevMap) e p)

findDefinition :: Parser.Identifier -> Model ([Parser.Identifier], Parser.Statement)
findDefinition identifier = do
    defns <- getDefinitions
    unpackDefn identifier (Map.lookup identifier defns)

unpackDefn :: Parser.Identifier -> Maybe ([Parser.Identifier], Parser.Statement) -> Model ([Parser.Identifier], Parser.Statement)
unpackDefn var Nothing = throwError ("Function " ++ var ++ " is not defined")
unpackDefn _ (Just result) = do
    return result

makeVector :: [Float] -> Vector
makeVector (x:y:z:[]) = VECTOR x y z
makeVector _ = error "invalid vector length"

rotateVectorByVector :: Float -> Vector -> Vector -> Vector
rotateVectorByVector theta vOfRot (VECTOR vx vy vz) = makeVector $ List.map (List.foldr (+) 0) (List.map (List.zipWith (*) [vx, vy, vz]) (getRotationMatrix vOfRot theta))

getRotationMatrix :: Vector -> Float -> [[Float]]
getRotationMatrix (VECTOR vx vy vz) theta =
    [[c+vx*vx*(1-c),     vx*vy*(1-c)-vz*s, vx*vz*(1-c)+vy*s],
     [vx*vy*(1-c)+vz*s,  c+vy*vy*(1-c),    vy*vz*(1-c)-vx*s],
     [vx*vz*(1-c)-vy*s,  vy*vz*(1-c)+vx*s, c+vz*vz*(1-c)   ]] 
     where
        radTheta = (theta*pi)/180
        c = cos radTheta
        s = sin radTheta

rotateX :: Float -> Model ()
rotateX theta = do
    (ORIENT vx vy vz) <- getOrientation
    rotate vx theta

rotateY :: Float -> Model ()
rotateY theta = do
    (ORIENT vx vy vz) <- getOrientation
    rotate vy theta

rotateZ :: Float -> Model ()
rotateZ theta = do
    (ORIENT vx vy vz) <- getOrientation
    rotate vz theta


rotate :: Vector -> Float -> Model ()
rotate v theta = do
    (ORIENT vx vy vz) <- getOrientation
    putOrientation (ORIENT (rotateFunc vx) (rotateFunc vy) (rotateFunc vz))
    where
        rotateFunc = rotateVectorByVector theta v


getOrientation :: Model Orientation
getOrientation = state $ \(TOTAL f e (PERSIST (HEADSTATE p o a) stk records)) ->  (o,(TOTAL f e (PERSIST (HEADSTATE p o a) stk records)))   

putOrientation :: Orientation -> Model ()
putOrientation orientation = state $ \(TOTAL f e (PERSIST (HEADSTATE p _ a) stk records)) ->  ((),(TOTAL f e (PERSIST (HEADSTATE p orientation a) stk records)))   

