module Declaration where

import qualified Data.Map as Map
import Data.List

import Syntax
import Expressions
import BooleanExpressions
import ESMC

-- TODO define kind x

evalDec :: (E, S, M, C) -> (E, S, M, C)
evalDec (e,s,m,c)
   | x == "const" = evalConst (e,s,m,tail c)
   | x == "var" = evalEVar (e,s,m,tail c)
   where x = head c
   
evalConst :: (E, S, M, C) -> (E, S, M, C)
evalConst (e,s,m,c)
   | x == "int" = evalConst (evalDecExp(e,s,m,tail c))
   | x == "boolean" = evalConst (evalDecBoolean(e,s,m,tail c))
   | x == ":=" = eval (f,tail s,m,tail c)
   | x == "fimElse" = eval (f,tail s,m,tail c)
   where x = head c
         f = Map.insert (head c) (head s) f

-- TODO get position of c in m to z
evalEVar :: (E, S, M, C) -> (E, S, M, C)
evalEVar (e,s,m,c)
   | x == "int" = evalConst (evalDecExp(e,s,m,tail c))
   | x == "boolean" = evalConst (evalDecBoolean(e,s,m,tail c))
   | x == ":=" = eval (f,tail s,Map.insert (head c) (head s) m,tail c)
   | x == "fimElse" = eval (f,tail s,Map.insert (head c) (head s) m,tail c)
   where x = head c
         f = Map.insert (head c) (z) f

first (_,s,_,_) = s	 
		 
evalDecExp :: (E, S, M, C) -> (E, S, M, C)
evalDecExp (e,s,m,c)
    | null c = (e,s,m,c)
    | x == "if" = evalExpIF (e,s,m,tail c)
    | x `elem` vars = evalExp (evalVar (e,s,m,c))
    | x == "+" = evalExp (evalPlus (e,s,m,c))
    | x == "-" = evalExp (evalMinus (e,s,m,c))
    | x == "*" = evalExp (evalMul (e,s,m,c))
    | isDigit (head x) = evalExp (e,x:s,m,tail c)
    | otherwise = (e,s,m,c)
    where x = head c
	
evalExpIF :: (E, S, M, C) -> (E, S, M, C)
evalExpIF (e,s,m,c)
    | x == "tt" = (e,v,m,":="(tail (dropWhile (/="fimElse") c)))
    | x == "ff" = evalDecExp(e,s,m,tail (dropWhile (/="else") c))
    where x = head (first (evalBoolean(e,s,m,takeWhile (/="then") c)))
          v = first evalDecExp(e,s,m,takeWhile(/="else"))
		  
evalDecBoolean :: (E, S, M, C) -> (E, S, M, C)
evalDecBoolean (e,s,m,c)
    | null c = (e,s,m,c)
    | x == "if" = evalBoolIF (e,s,m,tail c)
    | x == "tt" = evalBoolean (evalT (e,s,m,c))
    | x == "ff" = evalBoolean (evalT (e,s,m,c))
    | x == "=" = evalBoolean (evalEq (e,s,m,c))
    | x == "or" = evalBoolean (evalOr (e,s,m,c))
    | x == "~" = evalBoolean (evalNot (e,s,m,c))
    | x `elem` vars = evalBoolean (evalExp (e,s,m,c))
    | isDigit (head x) = evalBoolean (evalExp (e,s,m,c))
    | otherwise = (e,s,m,c)
    where x = head c
	
evalBoolIF :: (E, S, M, C) -> (E, S, M, C)
evalBoolIF (e,s,m,c)
    | x == "tt" = (e,v,m,":="(tail (dropWhile (/="fimElse") c)))
    | x == "ff" = evalDecBoolean(e,s,m,tail (dropWhile (/="else") c))
    where x = head (first (evalBoolean(e,s,m,takeWhile (/="then") c)))
          v = first evalDecBoolean(e,s,m,takeWhile(/="else"))
	