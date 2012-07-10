import MyLanguage.Values
import MyLanguage.Eval
import MyLanguage.Parser
import MyLanguage.BuiltIns
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
--import System
import System.Environment

main = do
    args <- getArgs
    parseEvalFile (head args)

parseEvalFile xs = do
    contents <- readFile xs
    case (parse parseMany "MyLanguage" contents) of
        Right e  -> printEval e
        Left err -> print err

printEval es = do
    e <- evalList' es (Context M.empty M.empty defaultHaskellFunctions 0)
    case e of
        Right (_,c) -> return ()
        Left err    -> print err
