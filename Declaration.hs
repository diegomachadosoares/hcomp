module Declaration where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Data.List
import Data.Char

import Syntax
import HelperTools
import Expressions

evalDecExp :: (E, S, M, C, O) -> (E, S, M, C, O)
evalDecExp (e,s,m,[],o) = (e,s,m,[],o)
evalDecExp (e,s,m,(Cexp (IfExp a b d)):c,o)= evalExpIF (e,s,m,(Cexp (IfExp a b d)):c,o)
evalDecExp (e,s,m,c,o)= evalExp(e,s,m,c,o)

evalExpIF :: (E, S, M, C, O) -> (E, S, M, C, O)
evalExpIF (e,s,m,(Cexp (IfExp a b d)):c,o)
    | x = (e,v,m,tail c,o)
    | otherwise = evalDecExp(e,s,m,tail c,o)
    where x = rBVal (head (filterS (evalExp(e,s,m,[Cexp a],o))))
          v = filterS (evalExp(e,s,m,[Cexp b],o))

evalDec :: (E, S, M, C, O) -> (E, S, M, C, O)
evalDec (e,s,m,[],o) = (e,s,m,[],o)
evalDec (e,s,m,(Ccom (Var a b d)):c,o) = (((Map.insert a (BndLoc (Loc ( Vector.length m)))) e),s,m Vector.++ (Vector.singleton (convValStr(head(filterS (evalDecExp(e,s,m,[Cexp d],o)))))),c,o)
evalDec (e,s,m,(Ccom (Const a b d)):c,o) = (Map.insert a (convValBnd(head (filterS (evalDecExp(e,s,m,[Cexp d],o))))) e,s,m,c,o)
evalDec (e,s,m,c,o) = (e,s,m,c,o)
