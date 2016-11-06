module OpenSCAD(getOpenSCADStr) where
import State
import Eval
import Control.Monad.State
import qualified Data.List as List

type Writer a = State (String, String) a

getOpenSCADStr :: [State.RecordedActions] -> String
getOpenSCADStr positions = snd $ evalState ((generateOpenSCAD positions) >> get) ("", "")

generateOpenSCAD :: [State.RecordedActions] -> Writer () -- output code could be simplified by merging adjacent unions/intersections
generateOpenSCAD ((State.MOVE, positions) : xs) = do
    generateOpenSCAD xs
generateOpenSCAD ((action, positions) : xs) = do
    writeAction action
    indentUp
    generateMotion positions
    indentDown
    putLine $ "}"
generateOpenSCAD [] = do
    return ()

writeAction :: State.Action -> Writer ()
writeAction action = case action of
    State.DRAW -> putLine $ "union {"
    State.ERASE -> putLine $ "intersection"
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
generateMotion positions = List.foldr1 (>>) (List.map (generateJoint) (List.zip positions (List.tail positions)))

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
    putLine $ "sphere(radius = " ++ (show r) ++ ");"
    indentDown
    putLine $ "}"