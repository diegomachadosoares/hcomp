-- Syntax.hs
module Syntax where

import qualified Data.Map as Map
import qualified Data.Vector as V

type E = Map.Map String (String,String)
type S = [String]
type M = V.Vector String
type C = [String]

-- | Memory Address of the variables
newtype Loc = Loc Int
    deriving (Show,Eq)

-- | Variable and Constant Values
newtype Value = Value Int
    deriving (Show,Eq)

-- | Environment recipient to hold constant values and variable addresses
data EnvVal =
  EnvLoc Loc
  | EnvVal Value
  deriving (Show,Eq)

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
