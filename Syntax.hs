-- Syntax.hs
module Syntax where

import qualified Data.Map as Map
import qualified Data.Vector as V

type E = Map.Map String Bnd
type S = [Value]
type M = V.Vector Str
type C = [Contr]
type Cexp = [Exp]

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

data Contr = Ccom Com | Cexp [Exp] |Cvar [Char] | CBool Bool
  deriving (Show,Eq)

data Com =
    While [Exp] [Contr]
  | If [Exp] [Contr] [Contr]
  | Attr String [Exp]
  | Var String String [Exp]
  | Const String String [Exp]
  | Nill
  deriving(Eq,Show)

data Exp =
   Num Int
 | Or
 | And Exp Exp
 | Eq
 | Lt
 | Gt
 | Add
 | Sub
 | Mul
 | Div
 | Not
 | NegInt Exp
 | Int Integer
 | EBool Bool
 | Evar [Char]
 | IfExp [Exp] [Exp] [Exp]
 | Null
  deriving (Eq,Ord,Show)
