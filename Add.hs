-- Add.hs
module Add (add) where

import Data.Char
import Data.String

data Tree a = Empty | Node a (Tree a) (Tree a) deriving(Eq,Ord,Show)

newtype PIdent =
  PIdent ((Int,Int),String)
  deriving(Eq,Ord,Show)

data Decl =
  Dec Type PIdent
  deriving(Eq,Ord,Show)

data Block =
    BlockC Com
  | BlockB [Com]
  deriving(Eq,Ord,Show)

data Com =
    Dec Decl
  | While Exp Block
  | If Exp Block
  | IfElse Exp Block Block
  | Return Exp
  deriving(Eq,Ord,Show)

data Exp =
   Atr PIdent Exp
 | Or Exp Exp
 | And Exp Exp
 | Eq Exp Exp
 | Neq Exp Exp
 | Lt Exp Exp
 | Gt Exp Exp
 | Add Exp Exp
 | Sub Exp Exp
 | Mul Exp Exp
 | Div Exp Exp
 | Neg Exp
 | NegInt Exp
 | Int Integer
 | Bool BoolT
  deriving (Eq,Ord,Show)

data BoolT =
    ETrue
  | EFalse
  deriving(Eq,Ord,Show)

data Type =
    TInt
  | TBool
  deriving(Eq,Ord,Show)

evalExp :: Expr -> Int
evalExp exp = case exp of
  Add x y -> eval x + eval y
  Sub x y -> eval x - eval y
  Mul x y -> eval x * eval y
  Div x y -> eval x / eval y
