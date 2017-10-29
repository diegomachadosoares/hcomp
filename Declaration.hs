module Declaration where

import qualified Data.Map as Map
import Data.List
import Expressions
import BooleanExpression

-- TODO define kind x

evalDec :: ([String], Map.Map String String, [String], x) -> ([String], Map.Map String String, [String], x)
evalDec (s,m,c,e)
   | x == "const" = evalConst (a,b,tail c,e)
   | x == "var" = evalVar (a,b,tail c,e)
   where x = head c
   
evalConst :: ([String], Map.Map String String, [String], x) -> ([String], Map.Map String String, [String], x)
evalConst (s,m,c,e)
   | x == "int" = evalConst (evalExp(a,b,tail c,e))
   | x == "boolean" = evalConst (evalBoolean(a,b,tail c,e))
   | x == ":=" = eval (tail s,m,tail c,f)
   where x = head c
         f = Map.insert (head c) (head s) f

-- TODO get position of c in m to z
evalVar :: ([String], Map.Map String String, [String], x) -> ([String], Map.Map String String, [String], x)
evalVar (s,m,c,e)
   | x == "int" = evalConst (evalExp(a,b,tail c,e))
   | x == "boolean" = evalConst (evalBoolean(a,b,tail c,e))
   | x == ":=" = eval (tail s,Map.insert (head c) (head s) m,tail c,f)
   where x = head c
         f = Map.insert (head c) (z) f