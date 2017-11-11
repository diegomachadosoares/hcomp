module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Syntax

{-
data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
            deriving (Show)

data BBinOp = And | Or deriving (Show)

data RBinOp = Greater | Less deriving (Show)

data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
             deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
              deriving (Show)

data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
            deriving (Show)
-}

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
                                    , ";"]
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
{-
sequenceOfStmt =
    do  list <- (sepBy1 statement' semi)
        -- If there's only one statement return it without using Seq.
        return $ if length list == 1 then head list else Seq list
-}
-- | TODO - Missing Declaration
statement' :: Parser Com
statement' =   seqStmt
           <|> whileStmt
           <|> assignStmt
           <|> ifStmt
           <|> varIStmt
           <|> varBStmt
           <|> constIStmt

ifStmt :: Parser Com
ifStmt =
    do  reserved "if"
        cond  <- aExpression
        reserved "then"
        stmt1 <- statement
        reserved "else"
        stmt2 <- statement
        reserved "end"
        return $ If cond [Ccom stmt1] [Ccom stmt2]

whileStmt :: Parser Com
whileStmt =
    do  reserved "while"
        cond <- aExpression
        reserved "do"
        stmt <- statement
        reserved "end"
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
        reserved ";"
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

aExpression :: Parser Exp
aExpression = buildExpressionParser aOperators aTerm
{-
bExpression :: Parser Exp
bExpression = buildExpressionParser bOperators bTerm
-}

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

{-
bTerm =  parens bExpression
     <|> (reserved "true"  >> return (EBool True ))
     <|> (reserved "false" >> return (EBool False))
     <|> rExpression


rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ Cexp op a1 a2

relation =   (reservedOp ">" >> return Gt)
         <|> (reservedOp "<" >> return Lt)
-}

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