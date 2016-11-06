module Main(run) where
import OpenSCAD
import Eval
import State
import Parser
import Control.Monad.Except

run :: String -> String
run str = OpenSCAD.getOpenSCADStr $
				State.evalModel $
					(Eval.executeProgram (Parser.parseProgram str)) >> State.getRecords
