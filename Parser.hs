module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Syntax

languageDef =
    emptyDef { Token.commentStart    = "/*"
        , Token.commentEnd      =   "*/"
        , Token.commentLine     =   "//"
        , Token.identStart      =   letter
        , Token.identLetter     =   alphaNum
        , Token.reservedNames   =   [ "if"
                                    , "then"
                                    , "else"
                                    , "while"
                                    , "do"
                                    , "true"
                                    , "false"
                                    , "var"
                                    , "{"
                                    , "}"
                                    , "const"
                                    , "int"
                                    , "bool"
                                    , "end"
                                    , "proc"
                                    , "("
                                    , ")"
                                    , ";"
                                    , "print"
                                    , "exit"
                                    , "func"
                                    , "call"
                                    , "callf"]
        , Token.reservedOpNames =   ["+", "-", "*", "/", ":="
                                    , "<", ">", "and", "or", "not","="
                                    ]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Com
whileParser = whiteSpace >> statement'


statement :: Parser Com
statement =   parens statement
            <|> statement'

-- | TODO - Missing Declaration
statement' :: Parser Com
statement' =   seqStmt
           <|> whileStmt
           <|> assignStmt
           <|> ifStmt
           <|> varIStmt
           <|> varBStmt
           <|> constIStmt
           <|> decProc
           <|> exitStmt
           <|> printStmt
           <|> callProc
           <|> decFunc

ifStmt :: Parser Com
ifStmt =
    do  reserved "if"
        cond  <- aExpression
        reserved "then"
        stmt1 <- statement
        reserved "else"
        stmt2 <- statement
        reserved "endIF"
        return $ If cond [Ccom stmt1] [Ccom stmt2]

whileStmt :: Parser Com
whileStmt =
    do  reserved "while"
        cond <- aExpression
        reserved "do"
        stmt <- statement
        reserved "endWhile"
        return $ While cond [Ccom stmt]

assignStmt :: Parser Com
assignStmt =
    do  var  <- identifier
        reservedOp ":="
        expr <- aExpression
        return $ Attr var expr

seqStmt :: Parser Com
seqStmt =
    do  reserved "{"
        stmt1 <- statement
        reserved "}"
        reserved ";"
        reserved "{"
        stmt2 <- statement
        reserved "}"
        return $ Sequence stmt1 stmt2

varIStmt :: Parser Com
varIStmt =
    do  reserved "var"
        var <- identifier
        reserved "int"
        reserved ":="
        expr <- aExpression
        return $ Var var "int" expr

varBStmt :: Parser Com
varBStmt =
    do  reserved "var"
        var <- identifier
        reserved "bool"
        expr <- aExpression
        return $ Var var "bool" expr

constIStmt :: Parser Com
constIStmt =
    do  reserved "const"
        var <- identifier
        reserved "int"
        reserved ":="
        expr <- aExpression
        return $ Const var "int" expr

decProc :: Parser Com
decProc =
    do  reserved "proc"
        name <- identifier
        reserved "("
        form <-forms
        reserved ")"
        reserved "begin"
        stmt <- statement
        reserved "end"
        return $ ProcR name form [Ccom stmt]

callProc :: Parser Com
callProc =
    do  reserved "call"
        name <- identifier
        reserved "("
        exps <- explist
        reserved ")"
        return $ ProcA name exps

decFunc :: Parser Com
decFunc =
    do  reserved "func"
        name <- identifier
        reserved "("
        form <-forms
        reserved ")"
        reserved "begin"
        exp <- aExpression
        reserved "end"
        return $ Func name form exp

callFunc :: Parser Exp
callFunc =
    do  reserved "callf"
        name <- identifier
        reserved "("
        exps <- explist
        reserved ")"
        return $ FunA name exps

printStmt :: Parser Com
printStmt =
    do  reserved "print"
        reserved "("
        exp <- aExpression
        reserved ")"
        return $ Print exp

exitStmt :: Parser Com
exitStmt =
    do  reserved "exit"
        number <- integer
        return $ Exit number

explist :: Parser [Exp]
explist =
    do  exp <- aExpression
        reserved ","
        f <- expA
        return $ exp:f

formsI :: Parser [String]
formsI =
    do  var <- identifier
        reserved "int"
        reserved ","
        f <- forms
        return $ var:f

formsB :: Parser [String]
formsB =
    do  var <- identifier
        reserved "bool"
        reserved ","
        f <- forms
        return $ var:f

formsN :: Parser [String]
formsN =
    do return ([])

expN :: Parser [Exp]
expN =
    do return ([])

forms = formsI <|> formsB <|> formsN
expA = explist <|> expN


aExpression :: Parser Exp
aExpression = bExpression <|> callFunc

bExpression :: Parser Exp
bExpression = buildExpressionParser aOperators aTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (NegInt  ))          ]
             , [Infix  (reservedOp "*"   >> return (Mul     )) AssocLeft
             ,  Infix  (reservedOp "/"   >> return (Div     )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (Add     )) AssocLeft
             ,  Infix  (reservedOp "-"   >> return (Sub     )) AssocLeft]
             , [Infix  (reservedOp "="   >> return (Eq      )) AssocLeft]
             , [Prefix (reservedOp "not" >> return (Not     ))          ]
             , [Infix  (reservedOp "or"  >> return (Or      )) AssocLeft]
             ]


aTerm =  parens aExpression
     <|> liftM Evar identifier
     <|> liftM Num integer

parseString :: String -> Com
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Com
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r