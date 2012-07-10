module MyLanguage.Values where

import qualified Data.Map as M
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Control.Monad.Error
import Text.RJson
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive

data Context = Context {
        contextVariables :: M.Map String Value, 
        contextFunctions :: M.Map String Expression, 
        contextHaskellFunctions :: M.Map String ([Value] -> IO Value), 
        contextLine :: Int
    }

instance Show Context where
    show (Context v f _ l) = (show $ M.toList v) ++ "\t" ++ (show $ M.toList f) ++ "\t" ++ show l

readContext c
    | c == ""   = Context M.empty M.empty M.empty 0
    | otherwise = Context (M.fromList $ read vars) (M.fromList $ read funcs) (M.empty) (read line)
    where vars  = takeWhile (/= '\t') c
          funcs = takeWhile (/= '\t') $ drop (length vars + 1) c
          line  = drop (length funcs + 1) $ drop (length vars + 1) c

instance Pretty Context where
    pretty (Context v _ _ l) = (foldl (\acc (n,v) -> acc ++ n ++ " = " ++ pretty v ++ "\n") "" $ M.toList v) ++ show l ++ " lines"

class Pretty a where
    pretty :: a -> String

class ShowType a where
    showType :: a -> String

data Expression = Assign Expression AssignOp Expression -- Name = Expression
                | Literal Value
                | Name String
                | Op Operand
                | Bracket [Expression]
                | If Expression [Expression] Expression
                | Else [Expression]
                | For Expression Expression Expression [Expression]
                | Foreach Expression Expression [Expression]
                | While Expression [Expression]
                | Function String [String] [Expression]
                | FunctionCall String [Expression]
                | Slice Expression Expression Expression -- list, first number, second number
                | Import String
    deriving (Eq, Ord, Show, Read)

instance Pretty Expression where
    pretty (Assign s o e)     = pretty s ++ pretty o ++ pretty e ++ "\n"
    pretty (Literal v)        = pretty v ++ " "
    pretty (Name s)           = s ++ " "
    pretty (Op o)             = pretty o ++ " "
    pretty (Bracket es)       = "( " ++ pretty es ++ ") "
    pretty (If e es e2)       = "if " ++ pretty e ++ " {\n" ++ pretty es ++ "}" ++ prettyExtraIf e2 ++ f
        where f
                  | e2 == Literal NothingValue = "\n"
                  | otherwise                  = ""
    pretty (Else es)           = " else {\n" ++ pretty es ++ "}\n"
    pretty (For a p i es)      = "for (" ++ prettyAssign a ++ "; " ++ pretty p ++ "; " ++ prettyAssign i ++ ") {\n" ++ pretty es ++ "}\n"
    pretty (Foreach a b es)    = "foreach (" ++ pretty a ++ "in " ++ pretty b ++ ") {\n" ++ pretty es ++ "}\n"
    pretty (While p es)        = "while (" ++ pretty p ++ ") {\n" ++ pretty es ++ "}\n"
    pretty (Function n as es)  = "function " ++ n ++ " (" ++ prettyArgList as ++ ") {\n" ++ pretty es ++ "}\n"
    pretty (FunctionCall n as) = n ++ " (" ++ prettyArgListCall as ++ ")\n"
    pretty (Slice n a b)       = pretty n ++ "[ " ++ pretty a ++ ": " ++ pretty b ++ "]"
    pretty (Import xs)         = "import " ++ xs ++ "\n"

prettyAssign a = init $ pretty a -- assign without newline

instance Pretty [Expression] where
    pretty es = foldl (\acc x -> acc ++ pretty x) "" es

prettyExtraIf (Literal NothingValue) = ""
prettyExtraIf i@(If _ _ _)           = " else " ++ pretty i
prettyExtraIf e@(Else _)             = pretty e

prettyArgList (a:[]) = a ++ " "
prettyArgList (a:as) = a ++ ", " ++ prettyArgList as

prettyArgListCall (a:[]) = pretty a ++ " "
prettyArgListCall (a:as) = pretty a ++ ", " ++ prettyArgListCall as 
data Operand = PlusOp
             | MinusOp
             | DivideOp
             | MultiplyOp
             | EqOp
             | NotEqOp
             | MoreEqOp
             | LessEqOp
             | MoreOp
             | LessOp
             | AndOp 
    deriving (Eq, Ord, Show, Enum, Read)

instance Pretty Operand where
    pretty x = fromMaybe "" $ lookup x prettyOps

data AssignOp = EqAssign 
              | PlusAssign
              | MinusAssign
              | DivideAssign
              | MultiplyAssign
    deriving (Show, Ord, Eq, Enum, Read)

instance Pretty AssignOp where
    pretty x = " " ++ (fromMaybe "" $ lookup x prettyAssignOps) ++ " "

prettyOps = zip [PlusOp .. AndOp] ["+","-","/","*","==","/=",">=","<=",">","<","&&"]

assignOpsToOps = zip [PlusAssign .. MultiplyAssign] [PlusOp .. MultiplyOp]

prettyAssignOps = zip [EqAssign .. MultiplyAssign] ["=","+=","-=","/=","*="]
anyOps  = [PlusOp, MultiplyOp, EqOp, NotEqOp, MoreEqOp, LessEqOp, MoreOp, LessOp]
boolOps = [AndOp]

data MError = UnboundVarError String Context 
            | UnboundFuncError String Context
            | TypeError Expression [Value] Context
            | ParseError String
            | DefaultError String Context
    
instance Show MError where
    show (UnboundVarError xs c)         = "Error: Could not find variable '" ++ xs ++ "'" ++ atLine c
    show (UnboundFuncError xs c)        = "Error: Could not find function '" ++ xs ++ "'" ++ atLine c
    show (DefaultError xs _)            = xs
    show (TypeError (Op o) xs c)        = showTypeError (fromMaybe "" $ lookup o prettyOps) xs c
    show (TypeError (If _ _ _ ) xs c)   = showTypeError "if" xs c
    show (TypeError (While _ _) xs c)   = showTypeError "while" xs c
    show (TypeError (Slice _ _ _) xs c) = showTypeError "slice" xs c 
    show (ParseError xs)                = xs

showTypeError os xs c = "Error: Function '" ++ os ++ "' does not take a " ++ (showType $ head xs) ++ (foldl f "" (tail xs)) ++ atLine c
    where f acc x = acc ++ " and a " ++ (showType x)

atLine c = "\nat line " ++ (show $ contextLine c)

instance Error MError where
    noMsg = DefaultError "An error has occurred" (Context M.empty M.empty M.empty 0)
    strMsg xs = DefaultError xs (Context M.empty M.empty M.empty 0)

type ThrowsError = Either MError

data Value = IntValue Int
    | DoubleValue Double
    | StringValue String
    | BoolValue Bool
    | ListValue [Expression] -- List of expressions allows for variable names
    | NothingValue
    deriving (Eq, Ord, Show, Read)


instance Pretty Value where
    pretty (IntValue x)     = show x
    pretty (DoubleValue x)  = show x
    pretty (StringValue xs) = '"':xs++['"']
    pretty (BoolValue x)    = show x
    pretty (ListValue xs)   = "[" ++ prettyArgListCall xs ++ "]"
    pretty NothingValue     = ""

instance ShowType Value where
    showType (IntValue _)    = "int"
    showType (StringValue _) = "string"
    showType (BoolValue _)   = "bool"
    showType NothingValue    = "nothing"
    
class (Eq a) => MValue a where
    toValue :: a -> Value
    fromValue :: Value -> a

instance MValue Int where
    toValue x = IntValue x
    fromValue (IntValue x) = x

instance MValue Double where
    toValue x =  DoubleValue x
    fromValue (DoubleValue x) = x

instance MValue String where
    toValue xs = StringValue xs
    fromValue (StringValue xs) = xs

instance MValue Bool where
    toValue x = BoolValue x
    fromValue (BoolValue x) = x

instance MValue [Expression] where
    toValue xs = ListValue xs
    fromValue (ListValue xs) = xs
