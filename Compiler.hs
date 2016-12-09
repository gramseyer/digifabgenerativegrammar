import OpenSCAD
import Eval
import State
import Parser
import Control.Monad.Except
import System.Environment
import System.IO

run :: String -> String
run str = OpenSCAD.getOpenSCADStr $
                State.evalModel $
                    (Eval.executeProgram (Parser.parseProgram str)) >> State.getRecords


processArgs :: [String] -> IO (String)
processArgs (name:[]) = do
    putStrLn $ "Compiling file \"" ++ name ++"\""
    return $ name
processArgs _ = do
    putStrLn "Usage: ./Compiler <filename>"
    error "invalid usage"

loadFile :: String -> IO (String, String)
loadFile filename = do
    contents <- readFile filename
    return (contents, filename ++ ".scad") -- TODO select output file name

outputFile :: (String, String) -> IO ()
outputFile (contents, filename) = do
    writeFile filename (run contents)

main = getArgs >>= processArgs >>= loadFile >>= outputFile
