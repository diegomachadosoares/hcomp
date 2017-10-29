module Declaration where

import qualified Data.Map as Map
import Data.List
import Expressions
import BooleanExpressions

-- TODO define kind x

evalDec :: (Map.Map Int String, [String], Map.Map String String, [String]) -> (Map.Map Int String, [String], Map.Map String String, [String])
evalDec (e,s,m,c)
   | x == "const" = evalConst (e,s,m,tail c)
   | x == "var" = evalEVar (e,s,m,tail c)
   where x = head c
   
evalConst :: (Map.Map Int String, [String], Map.Map String String, [String]) -> (Map.Map Int String, [String], Map.Map String String, [String])
evalConst (e,s,m,c)
   | x == "int" = evalConst (evalExp(e,s,m,tail c))
   | x == "boolean" = evalConst (evalBoolean(e,s,m,tail c))
   | x == ":=" = eval (f,tail s,m,tail c)
   where x = head c
         f = Map.insert (head c) (head s) f

-- TODO get position of c in m to z
evalEVar :: (Map.Map Int String, [String], Map.Map String String, [String]) -> (Map.Map Int String, [String], Map.Map String String, [String])
evalEVar (e,s,m,c)
   | x == "int" = evalConst (evalExp(e,s,m,tail c))
   | x == "boolean" = evalConst (evalBoolean(e,s,m,tail c))
   | x == ":=" = eval (f,tail s,Map.insert (head c) (head s) m,tail c)
   where x = head c
         f = Map.insert (head c) (z) f