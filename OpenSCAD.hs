module OpenSCAD(getOpenSCADStr) where
import State
import Eval
import Control.Monad.State
import qualified Data.List as List

type Writer a = State (String, String) a

getOpenSCADStr :: [State.RecordedActions] -> String
getOpenSCADStr positions = snd $ 
    evalState (((generateOpenSCAD.mergeRecords) positions) >> get) ("", "")

generateOpenSCAD :: [State.RecordedActions] -> Writer ()
generateOpenSCAD ((State.MOVE, positions) : xs) = do
    generateOpenSCAD xs
generateOpenSCAD ((action, positions) : xs) = do
    writeAction action
    indentUp
    generateOpenSCAD xs
    generateMotion positions
    indentDown
    putLine $ "}"
generateOpenSCAD [] = do
    writeAction State.DRAW
    putLine $ "}"

mergeRecords :: [State.RecordedActions] -> [State.RecordedActions]
mergeRecords ((MOVE, _) : actions) = mergeRecords actions
mergeRecords ((x, x') : (y, y') : rest) = 
    if x==y 
        then mergeRecords $ (x, x'++y'):rest
        else (x,x') : (mergeRecords $ (y,y'):rest)
mergeRecords [x] = [x]
mergeRecords [] = []

writeAction :: State.Action -> Writer ()
writeAction action = case action of
    State.DRAW -> putLine $ "union () {"
    State.ERASE -> putLine $ "intersection () {"
    _ -> error "move in action"

indentUp :: Writer ()
indentUp = do
    (indentation, text) <- get
    put (indentation ++ "  ", text)
    return ()

indentDown :: Writer ()
indentDown = do
    (indentation, text) <- get
    put (List.tail (List.tail indentation), text)
    return ()

putLine :: String -> Writer ()
putLine str = do
    (indent, prev) <- get
    put (indent, (prev ++ indent ++ str ++ "\n"))

generateMotion :: [State.Position] -> Writer ()
generateMotion positions = List.foldr1 (>>) $
    List.map (generateJoint) $
        List.filter filterRenderableObjects $
            List.zip positions (List.tail positions)

filterRenderableObjects :: (State.Position, State.Position) -> Bool
filterRenderableObjects ((v1, r1), (v2, r2)) = (r1>0.01 && r2>0.01)

generateJoint :: (State.Position, State.Position) -> Writer ()
generateJoint (p1, p2) = do
    putLine $ "hull() {"
    indentUp
    generateSphere p1
    generateSphere p2
    indentDown
    putLine $ "}"

generateSphere :: State.Position -> Writer ()
generateSphere ((x, y, z), r) = do
    putLine $ "translate(["++(show x)++","++(show y)++","++(show z)++"]) {"
    indentUp
    putLine $ "sphere(r = " ++ (show r) ++ ");"
    indentDown
    putLine $ "}"