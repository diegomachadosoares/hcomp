module Declaration where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Data.List
import Data.Char

import Syntax
import HelperTools
import Expressions

evalDec :: (E, S, M, C) -> (E, S, M, C)
evalDec (e,s,m,c)
   | x == "const" = evalConst (e,s,m,tail c)
   | x == "var" = evalEVar (e,s,m,tail c)
   | otherwise = (e,s,m,c)
   where x = head c
   
evalConst :: (E, S, M, C) -> (E, S, M, C)
evalConst (e,s,m,c)
   | x == "int" = evalConst (evalDecExp(e,s,m,tail c))
   | x == "boolean" = evalConst (evalDecExp(e,s,m,tail c))
   | x == ":=" = (Map.insert (head (tail c)) ("c",head s) e,tail s,m,tail (tail c))
   | x == "fimElse" = (Map.insert (head (tail c)) ("c",head s) e,tail s,m,tail (tail c))
   where x = head c

evalEVar :: (E, S, M, C) -> (E, S, M, C)
evalEVar (e,s,m,c)
   | x == "int" = evalEVar (evalDecExp(e,s,m,tail c))
   | x == "boolean" = evalEVar (evalDecExp(e,s,m,tail c))
   | x == ":=" = (Map.insert (head (tail c)) ("v", show ((Vector.length m))) e,tail s,m Vector.++ (Vector.singleton (head s)),tail (tail c))
   | x == "fimElse" = (Map.insert (head (tail c)) ("v", show ((Vector.length m))) e,tail s,m Vector.++ (Vector.singleton (head s)),tail (tail c))
   where x = head c

evalDecExp :: (E, S, M, C) -> (E, S, M, C)
evalDecExp (e,s,m,c)
    | null c = (e,s,m,c)
    | x == "if" = evalExpIF (e,s,m,tail c)
    | isDigit (head x) = evalExp (e,x:s,m,tail c)
    | otherwise = (e,s,m,c)
    where x = head c

evalExpIF :: (E, S, M, C) -> (E, S, M, C)
evalExpIF (e,s,m,c)
    | x == "tt" = (e,v,m,":=":(tail (dropWhile (/="fimElse") c)))
    | x == "ff" = evalDecExp(e,s,m,tail (dropWhile (/="else") c))
    where x = head (filterS (evalExp(e,s,m,takeWhile (/="then") c)))
          v = filterS (evalExp(e,s,m,(tail (dropWhile (/="then") (takeWhile(/="else") c)))))
