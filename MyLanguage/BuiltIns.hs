module MyLanguage.BuiltIns where
import qualified Data.Map as M
import MyLanguage.Values

builtInPrint ((StringValue s):[]) = putStrLn s >> return NothingValue
builtInPrint (v:[]) = putStrLn (pretty v) >> return NothingValue

defaultHaskellFunctions = M.fromList [("print", builtInPrint)]
