module MyLanguage.Parser where

import Text.ParserCombinators.Parsec
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import MyLanguage.Values

parseMany :: GenParser Char st [Expression]
parseMany = do
    e  <- parseLine
    spaces
    many $ char '\n'
    es <- (eof >> return []) <|> (parseMany)
    return $ e:es

parseAnother :: GenParser Char st [Expression]
parseAnother = do
    many1 $ char '\n'
    parseMany

parseLine :: GenParser Char st Expression
parseLine = do
    spaces
    exp <- try(parseAssign) <|> parseExpression
    return exp

parseExpression = do
    try(parseImport) <|> try(parseFunction) <|> try(parseIf) <|> try(parseForeach) <|> try(parseFor) <|> try(parseWhile) <|> try(parseMath) <|> try(parseListIndex) <|> try(parseSlice) <|>  try(parseFunctionCall) <|> parseList <|> parseLiteral <|> parseName <|> parseBracket

parseAssign :: GenParser Char st Expression
parseAssign = do
    name <- try(parseListIndex) <|> parseName
    spaces
    a <- string "=" <|> string "+=" <|> string "-=" <|> string "*=" <|> string "/="
    spaces
    exp <- try(parseMath) <|> try(parseListIndex) <|> try(parseSlice) <|> try(parseFunctionCall) <|> parseList <|> parseLiteral <|> parseName
    let o = fromMaybe EqAssign $ lookup a (reverseTuples prettyAssignOps)
    return $ Assign name o exp

parseIf :: GenParser Char st Expression
parseIf = do
    string "if"
    spaces
    b <- parseBracket
    spaces
    char '{'
    spaces
    many (char '\n')
    es <- parseInsideBrace
    char '}'
    spaces
    ex <- parseNextIf <|> (return $ Literal NothingValue)
    return (If b es ex)

parseElse = do
    char '{'
    spaces
    many (char '\n')
    es <- parseInsideBrace
    char '}'
    spaces
    return (Else es)

parseFor = do
    string "for"
    spaces
    char '('
    a <- parseAssign <|> (return $ Literal NothingValue)
    spaces
    char ';'
    spaces
    p <- parseMath
    spaces
    char ';'
    spaces
    e <- parseAssign
    spaces
    char ')'
    spaces
    char '{'
    spaces
    many (char '\n')
    es <- parseInsideBrace
    char '}'
    return (For a p e es)

parseForeach = do
    string "foreach"
    spaces
    char '('
    spaces
    n <- parseName
    many1 (char ' ')
    string "in"
    many1 (char ' ')
    exp <- parseName <|> parseList
    spaces
    char ')'
    spaces
    char '{'
    many (char '\n')
    es <- parseInsideBrace
    char '}'
    return (Foreach n exp es)

parseWhile = do
    string "while"
    spaces
    p <- parseBracket
    spaces
    char '{'
    spaces
    many (char '\n')
    es <- parseInsideBrace
    char '}'
    return (While p es)

parseNextIf = do
    spaces
    string "else"
    spaces
    parseIf <|> parseElse     

parseInsideBrace = do
    e <- parseLine <|> (return $ Literal NothingValue) 
    case e of
        Literal NothingValue -> return []
        _                    -> (spaces >> many (char '\n') >> parseInsideBrace >>= \ex -> return (e:ex))

parseFunction = do
    string "function"
    many1 (space)
    (Name n) <- parseName
    spaces
    char '('
    spaces
    args <- parseArguments
    spaces
    char ')'
    spaces
    char '{'
    spaces
    many (char '\n')
    es <- parseInsideBrace
    char '}'
    return (Function n args es)

parseArguments :: GenParser Char st [String]
parseArguments = do
    arg <- many1 letter
    sep <- (char ',') <|> (return ' ')
    case sep of
        ',' -> (spaces >> parseArguments >>= \e -> return (arg:e))
        ' ' -> return [arg]

parseFunctionCall = do
    (Name n) <- parseName
    char '('
    args <- parseCallArguments
    char ')'
    spaces
    return (FunctionCall n args)

parseCallArguments = do
--    arg <- (many1 letter >>= \s -> return (Name s)) <|> parseLiteral
    arg <- try(parseFunctionCall) <|> try(parseListIndex) <|> try(parseSlice) <|> try(parseName) <|> try(parseList) <|> parseLiteral
    sep <- (char ',') <|> (return ' ')
    case sep of
        ',' -> (spaces >> parseCallArguments >>= \e -> return (arg:e))
        ' ' -> return [arg]

parseList = do
    char '['
    spaces
    vs <- parseCallArguments
    spaces
    char ']'
    return (Literal $ ListValue vs)

parseListIndex = do
    n <- parseName
    char '['
    spaces
    i <- parseInt
    spaces
    char ']'
    return (Slice n (Literal i) $ Literal (IntValue $ (fromValue i)+1))

parseSlice = do
    n <- parseName
    char '['
    spaces
    a <- (parseInt >>= \e -> return $ Literal e) <|> parseName <|> (return $ Literal $ IntValue 0)
    spaces
    char ':'
    spaces 
    b <- (parseInt >>= \e -> return $ Literal e) <|> parseName <|> (return $ Literal NothingValue)
    spaces
    char ']'
    return (Slice n a b)

parseImport = do
    string "import "
    spaces
    n <- many1 letter
    return (Import n)

reverseTuples :: [(a,b)] -> [(b,a)]
reverseTuples xs = map (\(x,y) -> (y,x)) xs

parseLiteral :: GenParser Char st Expression
parseLiteral = do
    var <- parseString <|> parseInt
    return $ Literal var

parseMath :: GenParser Char st Expression
parseMath = do
    e  <- parseLiteral <|> parseName
    spaces
    o  <- parseOp
    spaces
    e2 <- parseExpression -- FIXME: adds brackets
    spaces
    return $ Bracket ([e, o, e2])

parseBracket = do
    char '('
    e <- parseMath
    char ')'
    return e

parseString :: GenParser Char st Value
parseString = do
    char '"'
    str <- many (noneOf "\"")
    char '"'
    return $ toValue str

parseInt :: GenParser Char st Value
parseInt = do
    d <- many1 digit
    return $ toValue ((read d) :: Int)

parseName :: GenParser Char st Expression
parseName = do
    n  <- letter
    ns <- many (letter <|> digit)
    return $ Name (n:ns)

parseMathWithoutBracket = do
    m <- parseMath
    case m of Bracket x -> return x

parseOp :: GenParser Char st Expression
parseOp = do
    op <- try(string "<=") <|> try(string ">=") <|> string "/=" <|> string "==" <|> string ">" <|> string "<" <|> string "+" <|> string "-" <|> string "*" <|> string "/" <|> string "&&"
    return $ Op (fromMaybe PlusOp $ lookup op (reverseTuples prettyOps))
