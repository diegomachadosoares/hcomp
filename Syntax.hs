-- Syntax.hs
module Syntax where

import qualified Data.Map as Map
import qualified Data.Vector as V

type E = Map.Map String Bnd
type S = [Value]
type M = V.Vector Str
type C = [Contr]

-- | Memory Address of the variables
data Loc = Loc Int
    deriving (Show,Eq)

-- | Variable and Constant Values
data Str = ValueI Int | ValueB Bool
    deriving (Show,Eq)

-- | Environment recipient to hold constant values and variable addresses
data Bnd =
  BndLoc Loc
  | BndVal Str
  deriving (Show,Eq)

data Value = ValI Int | ValB Bool | Com
  deriving (Show,Eq)

newtype PIdent =
  PIdent ((Int,Int),String)
  deriving(Eq,Ord,Show)

data Contr = Ccom Com | Cexp Exp |Cvar [Char] | CBool Bool
  deriving (Show,Eq)

data Decl =
  Dec Type PIdent
  deriving(Eq,Ord,Show)

data Block =
    BlockC Com
  | BlockB [Com]
  deriving(Eq,Ord,Show)

data Com =
    CDec Decl
  | While Exp Block
  | If Exp Block
  | IfElse Exp Block Block
  | Return Exp
  deriving(Eq,Ord,Show)

data Exp =
   Num Int
 | Atr PIdent Exp
 | Or Exp Exp
 | And Exp Exp
 | Eq Exp Exp
 | Neq Exp Exp
 | Lt
 | Gt
 | Add
 | Sub
 | Mul
 | Div
 | Neg
 | NegInt Exp
 | Int Integer
 | Bool BoolT
 | Null
  deriving (Eq,Ord,Show)

data BoolT =
    ETrue
  | EFalse
  deriving(Eq,Ord,Show)

data Type =
    TInt
  | TBool
  deriving(Eq,Ord,Show)
