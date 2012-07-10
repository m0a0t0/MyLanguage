import MyLanguage.Values
import MyLanguage.Eval
import MyLanguage.Parser
import MyLanguage.BuiltIns
import Text.ParserCombinators.Parsec
import qualified Data.Map as M

myAdd :: [Value] -> Value
myAdd ((IntValue x):(IntValue y):(IntValue z):[]) = toValue $ x + y + z


main = do
    let e = If (Bracket [toLitInt 7, Op LessEqOp, toLitInt 6]) [Assign "a" EqAssign (toLitInt 1)] $ If (Bracket [toLitInt 7, Op LessEqOp, toLitInt 5]) [Assign "a" EqAssign (toLitInt 2)] (Else [Assign "a" EqAssign (toLitInt 3)])
        s = ["a = 0",
             "for (i=0;i<10;i+=1) {",
             "a += 1",
             "}",
             "if (a <= 10) {",
             "a = \"It works\"",
             "}",{-else {",
             "a = a",
             "}",-}
             "b = 1",
             "while (b <= 10) {",
             "b += 1",
             "}"
            ]
        f = ["a = 1",
             "function add (a,b,c) {",
             "a += b",
             "a += c",
             "}",
             "add(a,1,1)",
             "b = [1,2,a]",
             "b += 17",
             "c = b[1:3]",
             "d = b[1:]",
             "e = b[1]"
            ]
        g = ["a = [1,2,3,4]",
             "foreach (n in a) {",
             "a += n",
             "}",
             "print(\"Hello, world\")",
             "print(1)",
             "print(a)"
            ]
--    putStrLn $ pretty e
--    print $ evalList [e]
--    putStrLn $ unlines s
    case (parse parseMany "blah" (unlines g)) of
        Right e  -> printEval e
        Left err -> print err

printEval es = do
    putStrLn (pretty es)
    print es
    e <- evalList' es (Context M.empty M.empty defaultHaskellFunctions 0)
    case e of
        Right (_,c) -> putStrLn $ pretty c
        Left err    -> print err
