import MyLanguage.Values
import MyLanguage.Eval
import MyLanguage.Parser
import qualified Data.Map as M
--import Control.Monad.State
import Text.ParserCombinators.Parsec
import System.IO

forever x = do
    x
    forever x

repl c = do
    putStr ">>> "
    hFlush stdout
    xs <- getLine
--    let xs' = replace ';' '\n' xs
    (s,c') <- parseAndEval (xs++"\n") c
    case s of "" -> return ()
              x  -> putStrLn x
    repl c'

replace c c' xs = map (\x -> if x == c then c' else x) xs

parseAndEval :: String -> Context -> IO (String, Context)
parseAndEval xs c = case (parse parseMany "repl" xs) of
                      Left err -> return (show err, c)
                      Right ys -> checkEval ys c

checkEval :: [Expression] -> Context -> IO (String, Context)
checkEval xs c = do
    e <- evalList' xs c
    case e of
        Left err -> return $ (show err, c)
        Right xs -> return $ (pretty $ fst xs, snd xs)
main = do
    repl (Context M.empty M.empty M.empty 0)
