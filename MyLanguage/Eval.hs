module MyLanguage.Eval (eval, evalList, evalList', toLitInt) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import MyLanguage.Values
import MyLanguage.Parser
import Text.ParserCombinators.Parsec
import Control.Monad.State 
import Control.Monad.Error
import Control.Monad.Identity

type Eval a = StateT Context (ErrorT MError IO) a

evalList :: [Expression] -> IO (Either MError (Value, Context))
evalList xs = evalList' xs (Context M.empty M.empty M.empty 0)
evalList' :: [Expression] -> Context -> IO (Either MError (Value, Context))
evalList' xs c = foldl f (return $ Right (NothingValue, c)) xs
f acc x = do 
    acc' <- acc
    case acc' of Left err    -> return $ Left err
                 Right (_,m) -> f' m x
f' :: Context -> Expression -> IO (Either MError (Value, Context))
f' acc x = do
    let y = contextLine acc + 1
    mEval x (acc {contextLine=y})

incLineN = do
    c <- get
    let l = contextLine c + 1
    put $ c {contextLine=l}

mEval :: Expression -> Context -> IO (Either MError (Value, Context))
mEval x s = runErrorT $ runStateT (eval x) s

getVariables :: Eval (M.Map String Value)
getVariables = liftM contextVariables get

getFuncs :: Eval (M.Map String Expression)
getFuncs = liftM contextFunctions get

getHaskellFuncs = liftM contextHaskellFunctions get

getLineN :: Eval Int
getLineN = liftM contextLine get

eval :: Expression -> Eval Value
eval (Assign (Name s) o e) = do
    v <- eval e
    assign s o v
    return NothingValue
eval (Assign s@(Slice n@(Name n') (Literal (IntValue a)) (Literal (IntValue b))) EqAssign e) = do
    n <- eval n
    c <- get
    case n of
        ListValue _ -> return ()
        n''         -> throwError (TypeError s [n''] c)
    v <- eval e
    let (ListValue xs) = n
        v' = ListValue (take a xs ++ [Literal v] ++ drop (a+1) xs)
    m <- getVariables
    let m' = M.insert n' v' m
    put (c {contextVariables=m'})
    return NothingValue
eval (Name s) = do
    m <- getVariables
    case M.lookup s m of
        Nothing -> get >>= \c -> throwError (UnboundVarError s c)
        Just x  -> return x
eval i@(If e es e2) = do
    e' <- eval e
    case e' of
        BoolValue b -> evalIf b es e2
        v           -> get >>= \c -> throwError $ TypeError i [v] c
eval (Else es)    = evalEvals es
eval f@(For a p e es) = do
    eval a
    evalFor p e es
    m <- getVariables
    c <- get
    incLineExpression f
    case a of
        Assign (Name n) _ _ -> put (c {contextVariables=M.delete n m})
    return NothingValue
eval f@(Foreach a b es) = do
    incLineExpression f
    b' <- eval b
    evalForeach a b' es
    return NothingValue
eval w@(While p es) = do
    c <- get
    incLineExpression w
    evalWhile w
eval (Bracket xs) = evalMath xs
eval (Literal (ListValue xs)) = do
    xs' <- evalIntoList xs
    return (ListValue (map Literal xs'))
eval (Literal v)  = return v
eval f@(Function n _ _) = do
    incLineExpression f
    m <- getFuncs
    c <- get
    let v = M.insert n f m
    put $ c {contextFunctions=v}
    return NothingValue
eval fc@(FunctionCall n es) = do
    m <- getFuncs
    h <- getHaskellFuncs
    c <- get
    case M.lookup n m of
        Just fu@(Function _ _ _) -> evalFunc fu fc
        Nothing                  -> case M.lookup n h of Just f  -> evalHaskellFunction f es
                                                         Nothing -> throwError $ UnboundFuncError n c
eval s@(Slice n a b) = do
    n' <- eval n
    c <- get
    let t x = TypeError s [x] c
    case n' of
        ListValue _ -> return NothingValue
        v           -> throwError $ t v
    a' <- eval a
    case a' of
        IntValue _ -> return NothingValue
        v          -> throwError $ t v
    b' <- eval b
    case b' of
        IntValue _   -> return (ListValue $ take (fromValue b'-fromValue a') $ drop (fromValue a') (fromValue n'))
        NothingValue -> return (ListValue $ take (lengthOfListValue n'-fromValue a') $ drop (fromValue a') (fromValue n'))
        v            -> throwError $ t v
eval (Import n) = do
    conts <- liftIO (readFile $ n++".ml")
    case parse parseMany "MyLanguage" conts of
        Right e  -> evalImport e 
        Left err -> throwError $ ParseError (show err)

evalImport :: [Expression] -> Eval Value
evalImport e = do
    c <- get
    e' <- liftIO $ evalList' e (c {contextLine=0})
    let oldLine = contextLine c
    case e' of
        Right (v,c') -> put (c' {contextLine=oldLine}) >> return NothingValue
        Left err     -> throwError err

lengthOfListValue (ListValue xs) = length xs

evalHaskellFunction f es = do
    es' <- evalIntoList es 
    liftIO (f es') 

evalForeach (Name n) (ListValue (x:xs)) es = do
    vars <- getVariables
    c <- get
    x' <- eval x
    let vars' = M.insert n x' vars
    put $ c {contextVariables=vars'}
    evalEvals es
    evalForeach (Name n) (ListValue xs) es
evalForeach (Name n) (ListValue []) _ = do
    vars <- getVariables
    c    <- get
    let m = M.delete n vars
    put $ c {contextVariables=m}
    return ()

evalEvals xs = evalEvals' xs False
evalEvals' (x:[]) b = do
    if b then incLineN else return ()
    eval x
evalEvals' (x:xs) b = do
    if b then incLineN else return ()
    eval x
    evalEvals' xs b

evalIf :: Bool -> [Expression] -> Expression -> Eval Value
evalIf b es e2 = do
    if b then evalEvals es else eval e2
    incLineExpression (If (Literal $ BoolValue b) es e2)
    return NothingValue

evalFor p i es = do
    p' <- eval p
    case p' of
        BoolValue True  -> evalEvals' es False >> evalFor' p i es
        BoolValue False -> return NothingValue
evalFor' p i es = do
    c <- get
    eval i
    p' <- eval p
    case p' of
        BoolValue True  -> evalEvals' es False >> evalFor' p i es
        BoolValue False -> put c >> return NothingValue

evalWhile w@(While p es) = do
    c <- get
    b <- eval p
    case b of
        BoolValue True  -> evalEvals es >> evalWhile w
        BoolValue False -> return NothingValue
        x               -> throwError $ TypeError w [x] c

evalFunc (Function _ as es) (FunctionCall _ as') = do
    c <- get
    as'' <- evalIntoList as'
    let m = M.fromList $ zip as as''
    put $ Context m (contextFunctions c) (contextHaskellFunctions c) (contextLine c)
    r <- evalEvals es
    c' <- get
    put c
    insertBack c' as as'
    return r
    
insertBack c (a:as) (a':as') = do
    case a' of
        Name s -> insertBackIsName s c (a:as) (a':as')
        _      -> return ()
insertBack a b c = liftIO (putStrLn $ show a ++ show b ++ show c)

insertBackIsName s c (a:as) (a':as') = do
    c' <- get
    let m = M.insert s (fromMaybe NothingValue $ M.lookup a (contextVariables c)) (contextVariables c')
    put $ c' {contextVariables=m}
    insertBack c as as'

evalIntoList [] = return []
evalIntoList (e:es) = do
    e'  <- eval e
    es' <- evalIntoList es
    return (e':es')

incLineExpression (If _ es e)            = incLineExpression' es >> incLineExpression e
incLineExpression (Else es)              = incLineExpression' es
incLineExpression (For _ _ _ es)         = incLineExpression' es
incLineExpression (Foreach _ _ es)       = incLineExpression' es
incLineExpression (While _ es)           = incLineExpression' es
incLineExpression (Function _ _ es)      = incLineExpression' es
incLineExpression (Literal NothingValue) = return ()

incLineExpression' es = doN (length es + 1) incLineN -- one extra for brace

doN 1 f = f
doN x f = do
    f
    doN (x-1) f

assign :: String -> AssignOp -> Value -> Eval Value
assign s EqAssign v = do
    m <- getVariables
    c <- get
    let m' = M.insert s v m
    put $ c {contextVariables=m'}
    return NothingValue
assign s o v = do
    m <- getVariables
    c <- get
    let x = fromMaybe NothingValue $ M.lookup s m
    v <- opToFunc (Op $ fromMaybe PlusOp (lookup o assignOpsToOps)) x v -- lookup given op in map and apply the old value and the new value to it
    let m' = M.insert s v m
    put $ c {contextVariables=m'}
    return NothingValue

evalMath :: [Expression] -> Eval Value
evalMath (e:o:e2:[]) = do  -- single sum
    e'  <- eval e
    e2' <- eval e2
    opToFunc o e' e2'
evalMath (e:o:e2:o2:e3:xs) 
    | precedence o >= precedence o2 = evalMathPrec e2 o2 e3 o e xs -- o2 should be evaluated before o
    | otherwise                     = evalMathPrec e o e2 o2 e3 xs
evalMath (x:[]) = eval x

evalMathPrec fe fo fe2 so se xs = do
    fe'  <- eval fe
    fe2' <- eval fe2
    se'  <- evalMath (se:xs)
    v  <- opToFunc fo fe' fe2'
    opToFunc so v se'

opToFunc :: Expression -> Value -> Value -> Eval Value
opToFunc o'@(Op o) = anyOpToFunc o'
{-    | o `elem` anyOps  = anyOpToFunc o' x y
    | o `elem` boolOps = return $ boolOpToFunc o' (fromValue x) (fromValue y)
    | otherwise        = return $ intOpToFunc o' (fromValue x) (fromValue y)-}

intOpToFunc :: Expression -> (Int -> Int -> Value)
intOpToFunc (Op PlusOp)     = baseOpToFunc (+)
intOpToFunc (Op MinusOp)    = baseOpToFunc (-)
intOpToFunc (Op DivideOp)   = baseOpToFunc div
intOpToFunc (Op MultiplyOp) = baseOpToFunc (*)

anyOpToFunc :: Expression -> Value -> Value -> Eval Value
anyOpTofunc o@(Op PlusOp)     (IntValue x) (IntValue y)         = return $ intOpToFunc o x y
anyOpToFunc (Op PlusOp)       (StringValue xs) (StringValue ys) = return $ StringValue $ xs ++ ys
anyOpToFunc (Op PlusOp)       (ListValue xs) v                  = return (ListValue $ xs ++ [Literal v])
anyOpToFunc o@(Op MultiplyOp) (IntValue x) (IntValue y)         = return $ intOpToFunc o x y
anyOpToFunc o@(Op MultiplyOp) x@(IntValue _) xs@(StringValue _) = anyOpToFunc o xs x
anyOpToFunc (Op MultiplyOp)   (StringValue s) (IntValue x)      = return $ StringValue $ concat $ replicate x s
anyOpToFunc  o                (IntValue x) (IntValue y)         = return $ intOpToFunc o x y
anyOpToFunc (Op PlusOp)       (DoubleValue x) (DoubleValue y)   = return $ baseOpToFunc (+)  x y
anyOpToFunc (Op MinusOp)      (DoubleValue x) (DoubleValue y)   = return $ baseOpToFunc (-)  x y
anyOpToFunc (Op DivideOp)     (DoubleValue x) (DoubleValue y)   = return $ baseOpToFunc (/)  x y
anyOpToFunc (Op MultiplyOp)   (DoubleValue x) (DoubleValue y)   = return $ baseOpToFunc (*)  x y
anyOpToFunc (Op EqOp)         x y                               = return $ baseOpToFunc (==) x y
anyOpToFunc (Op NotEqOp)      x y                               = return $ baseOpToFunc (/=) x y
anyOpToFunc (Op MoreEqOp)     x y                               = return $ baseOpToFunc (>=) x y
anyOpToFunc (Op LessEqOp)     x y                               = return $ baseOpToFunc (<=) x y
anyOpToFunc (Op MoreOp)       x y                               = return $ baseOpToFunc (>)  x y
anyOpToFunc (Op LessOp)       x y                               = return $ baseOpToFunc (<)  x y
anyOpToFunc o@(Op _)          x y                               = get >>= \c -> throwError (TypeError o [x,y] c)

boolOpToFunc (Op AndOp) = baseOpToFunc (&&)

baseOpToFunc op x y = toValue $ op x y

precedence (Op DivideOp)   = 2
precedence (Op MultiplyOp) = 3
precedence (Op PlusOp)     = 4
precedence (Op MinusOp)    = 5
precedence (Op EqOp)       = 6
precedence (Op NotEqOp)    = 6
precedence (Op AndOp)      = 7

toLitInt x = Literal $ IntValue x
