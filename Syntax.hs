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

data Contr = Ccom Com | Cexp Exp | Cvar String | CBool Bool
        |ADD
        |SUB
        |MUL
        |EQU
        |NOT
        |OR

  deriving (Show,Eq)

data Com =
    While Exp [Contr]
  | If Exp [Contr] [Contr]
  | Attr String Exp
  | Var String String Exp
  | Const String String Exp
  | Nill
  deriving(Eq,Show)

data Exp =
   Num Int
 | Or Exp Exp
 | Eq Exp Exp
 | Lt Exp Exp
 | Gt Exp Exp
 | Add Exp Exp
 | Sub Exp Exp
 | Mul Exp Exp
 | Div Exp Exp
 | Not Exp
 | NegInt Exp
 | Int Integer
 | EBool Bool
 | Evar String
 | IfExp Exp Exp Exp
 | Null
  deriving (Eq,Ord,Show)
