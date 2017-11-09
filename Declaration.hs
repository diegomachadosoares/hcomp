module Declaration where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Data.List
import Data.Char

import Syntax
import HelperTools
import Expressions

{-
evalConst :: (E, S, M, C) -> (E, S, M, C)
evalConst (e,s,m,(Ccom (Const a b d)):c)
   | x == "int" = evalConst (evalDecExp(e,s,m,tail c))
   | x == "boolean" = evalConst (evalDecExp(e,s,m,tail c))
   | x == ":=" = (Map.insert (head (tail c)) ("c",head s) e,tail s,m,tail (tail c))
   | x == "fimElse" = (Map.insert (head (tail c)) ("c",head s) e,tail s,m,tail (tail c))
   where x = head c
         v = filterS (evalDecExp(e,s,m,d))
evalEVar :: (E, S, M, C) -> (E, S, M, C)
evalEVar (e,s,m,c)
   | x == "int" = evalEVar (evalDecExp(e,s,m,tail c))
   | x == "boolean" = evalEVar (evalDecExp(e,s,m,tail c))
   | x == ":=" = (Map.insert (head (tail c)) ("v", show ((Vector.length m))) e,tail s,m Vector.++ (Vector.singleton (head s)),tail (tail c))
   | x == "fimElse" = (Map.insert (head (tail c)) ("v", show ((Vector.length m))) e,tail s,m Vector.++ (Vector.singleton (head s)),tail (tail c))
   where x = head c
-}

evalDecExp :: (E, S, M, C) -> (E, S, M, C)
evalDecExp (e,s,m,[]) = (e,s,m,[])
evalDecExp (e,s,m,(Cexp (IfExp a b d)):c)= evalExpIF (e,s,m,(Cexp (IfExp a b d)):c)
evalDecExp (e,s,m,c)= evalExp(e,s,m,c)

evalExpIF :: (E, S, M, C) -> (E, S, M, C)
evalExpIF (e,s,m,(Cexp (IfExp a b d)):c)
    | x = (e,v,m,tail c)
    | otherwise = evalDecExp(e,s,m,tail c)
    where x = rBVal (head (filterS (evalExp(e,s,m,[Cexp (a)]))))
          v = filterS (evalExp(e,s,m,[Cexp b]))

evalDec :: (E, S, M, C) -> (E, S, M, C)
evalDec (e,s,m,[]) = (e,s,m,[])
evalDec (e,s,m,(Ccom (Var a b d)):c) = (((Map.insert a (BndLoc (Loc ( Vector.length m)))) e),s,m Vector.++ (Vector.singleton (convValStr(head(filterS (evalDecExp(e,s,m,[Cexp d])))))),(Ccom (Var a b d)):c)
evalDec (e,s,m,(Ccom (Const a b d)):c) =(Map.insert a (convValBnd(head (filterS (evalDecExp(e,s,m,[Cexp d]))))) e,s,m,(Ccom (Const a b d)):c)
evalDec (e,s,m,c) = (e,s,m,c)
