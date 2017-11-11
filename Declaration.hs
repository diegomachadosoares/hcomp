module Declaration where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Data.List
import Data.Char

import Syntax
import HelperTools
import Expressions

evalDecExp :: (E, S, M, C) -> (E, S, M, C)
evalDecExp (e,s,m,[]) = (e,s,m,[])
evalDecExp (e,s,m,(Cexp (IfExp a b d)):c)= evalExpIF (e,s,m,(Cexp (IfExp a b d)):c)
evalDecExp (e,s,m,c)= evalExp(e,s,m,c)

evalExpIF :: (E, S, M, C) -> (E, S, M, C)
evalExpIF (e,s,m,(Cexp (IfExp a b d)):c)
    | x = (e,v,m,tail c)
    | otherwise = evalDecExp(e,s,m,tail c)
    where x = rBVal (head (filterS (evalExp(e,s,m,[Cexp a]))))
          v = filterS (evalExp(e,s,m,[Cexp b]))

evalDec :: (E, S, M, C) -> (E, S, M, C)
evalDec (e,s,m,[]) = (e,s,m,[])
evalDec (e,s,m,(Ccom (Var a b d)):c) = (((Map.insert a (BndLoc (Loc ( Vector.length m)))) e),s,m Vector.++ (Vector.singleton (convValStr(head(filterS (evalDecExp(e,s,m,[Cexp d])))))),c)
evalDec (e,s,m,(Ccom (Const a b d)):c) = (Map.insert a (convValBnd(head (filterS (evalDecExp(e,s,m,[Cexp d]))))) e,s,m,c)
evalDec (e,s,m,c) = (e,s,m,c)
