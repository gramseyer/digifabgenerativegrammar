module State (Model, RecordedActions, Action (DRAW, MOVE, ERASE), Vector, Position, evalModel,getRecords, runSubmodel, rotateX, rotateY, rotateZ, moveDiscretised, setMove, setDraw, setErase, initialState, getHeadState, pushHeadToStack, popHeadFromStack, getVarBindings, pushVarBindings, popVarBindings, addDefinitions, findDefinition) where
import Parser
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import qualified Data.List as List

data HeadState = HEADSTATE Position Orientation Action

data Action = DRAW | ERASE | MOVE deriving (Show, Eq)

-- There might be better libraries for these
data Orientation = ORIENT Vector Vector Vector

type Vector = (Float, Float, Float)

type Position = (Vector, Float)-- x,y,z radius

-- State that persists through control flow
data PersistState = PERSIST HeadState [HeadState] Recording

--Record of motion of the head through space.  
--Action, along with the discretized list of positions of the head
--as the motion was recorded.
type Recording = ([Position], [RecordedActions]) 

type RecordedActions = (Action, [Position])

-- State that is fixed at the start of evaluation, like function definitions
data FixedState = FIXED (Map.Map Parser.Identifier ([Parser.Identifier], Parser.Statement))

-- State that doesn't persist after the end of a statement - like function parameter bindings
-- Variable bindings are only introduced in expr evaluation (t) and function calls
data EphemeralState = EPHEMERAL [Map.Map Parser.Identifier Float]

data TotalState = TOTAL FixedState EphemeralState PersistState

type Model a = ExceptT String (State TotalState) a

evalModel :: Model a -> a
evalModel model = case fst.runModel $ model of
    Left errorMsg -> error errorMsg
    Right x -> x

runSubmodel :: ([RecordedActions] -> [RecordedActions]) -> Model () -> Model ()
runSubmodel transformFunc submodel = do
    headState <- getHeadState
    addRecords $ transformFunc (evalModelOnState submodel headState)

evalModelOnState :: Model () -> HeadState -> [RecordedActions]
evalModelOnState model headState = case 
    fst $ runState (runExceptT (model>>getRecords)) (makeInitialState headState)
    of
        Left errorMsg -> error errorMsg -- could thread this back to the main model
        Right x -> x

runModel :: Model a -> (Either String a, TotalState)
runModel model = runState (runExceptT model) initialState

initialHeadState :: HeadState
initialHeadState = HEADSTATE ((0,0,0), 0) (ORIENT (1,0,0) (0,1,0) (0,0,1)) DRAW

makeInitialState :: HeadState -> TotalState
makeInitialState h = TOTAL (FIXED Map.empty) (EPHEMERAL [Map.empty]) (PERSIST h [] ([],[]))

initialState :: TotalState
initialState = makeInitialState initialHeadState

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
makeVector (x:y:z:[]) = (x,y,z)
makeVector _ = error "invalid vector length"

makeList :: Vector -> [Float]
makeList (x,y,z) = [x,y,z]

rotateVectorByVector :: Float -> Vector -> Vector -> Vector
rotateVectorByVector theta vOfRot (vx, vy, vz) = makeVector $ List.map (List.foldr (+) 0) (List.map (List.zipWith (*) [vx, vy, vz]) (getRotationMatrix vOfRot theta))

getRotationMatrix :: Vector -> Float -> [[Float]]
getRotationMatrix (vx,vy,vz) theta =
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

adjustForOrientation :: Orientation -> Vector -> Vector
adjustForOrientation (ORIENT vx vy vz) v = makeVector $
    List.foldr (List.zipWith (+)) [0,0,0] $
        List.zipWith (\k->List.map ((*)k)) (makeList v)  [makeList vx, makeList vy, makeList vz]

addV :: Vector -> Vector -> Vector
addV (vx, vy, vz) (vx', vy', vz') = (vx+vx', vy+vy', vz+vz')

addPoses :: Position -> Position -> Position
addPoses (v, r) (v', r') = ((addV v v'), (r+r'))

logPos :: Position-> Model ()
logPos (v, r) = do
    (HEADSTATE base o _) <- getHeadState
    pushLogToStk $ addPoses base ((adjustForOrientation o v), r)

pushLogToStk :: Position -> Model ()
pushLogToStk newPos = state $ \(TOTAL f e (PERSIST h stk (recording, recorded))) ->  ((),(TOTAL f e (PERSIST h stk (newPos: recording, recorded))))   

endLogPosSet :: Model ()
endLogPosSet = state $ \(TOTAL f e (PERSIST (HEADSTATE p o a) stk (recording, recorded))) ->  ((),(TOTAL f e (PERSIST (HEADSTATE p o a) stk ([], (a, recording):recorded))))   

setPos :: Position -> Model ()
setPos pos = state $ \(TOTAL f e (PERSIST (HEADSTATE _ o a) stk records)) ->  ((),(TOTAL f e (PERSIST (HEADSTATE pos o a) stk records)))   

moveDiscretised :: [Position] -> Model ()
moveDiscretised poses = do
    mergedPositions
    endLogPosSet
    (HEADSTATE curPos o _) <- getHeadState
    setPos (addPoses curPos (adjustForOrientation o lastPos, r))
    where
        posModels = List.map logPos poses
        mergedPositions = List.foldr1 (>>) (posModels)
        (lastPos, r) = List.last poses

setAction :: Action -> Model ()
setAction action = state $ \(TOTAL f e (PERSIST (HEADSTATE p o _) stk records)) ->  ((),(TOTAL f e (PERSIST (HEADSTATE p o action) stk records)))

setDraw :: Model ()
setDraw = setAction DRAW

setErase :: Model ()
setErase = setAction ERASE

setMove :: Model ()
setMove = setAction MOVE

getRecords :: Model [RecordedActions]
getRecords = state $ \(TOTAL f e (PERSIST (HEADSTATE p o a) stk (recording, records))) ->  (records ,(TOTAL f e (PERSIST (HEADSTATE p o a) stk (recording, records))))  

addRecords :: [RecordedActions] -> Model ()
addRecords newRecords = state $ \(TOTAL f e (PERSIST (HEADSTATE p o a) stk (recording, records))) ->  (() ,(TOTAL f e (PERSIST (HEADSTATE p o a) stk (recording, newRecords ++ records))))  
