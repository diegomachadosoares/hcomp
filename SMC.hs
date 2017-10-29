module SMC where

import qualified Data.Map as Map
import Data.Char

import Expressions
import BooleanExpressions
import Commands

eval :: ([String], Map.Map String String, [String]) -> ([String], Map.Map String String, [String])
eval (s,m,c)
   | null c = (s,m,c)
   | x == "v" = evalExp (s,m,c)
   | x == "+" = evalExp (s,m,c)
   | x == "-" = evalExp (s,m,c)
   | x == "*" = evalExp (s,m,c)
   | x == "nil" = evalCMD (s,m,c)
   | x == ":=" = evalCMD (s,m,c)
   | x == "if" = evalCMD (s,m,c)
   | x == "tt" = evalBoolean (s,m,c)
   | x == "ff" = evalBoolean (s,m,c)
   | x == "=" = evalBoolean (s,m,c)
   | x == "or" = evalBoolean (s,m,c)
   | x == "~" = evalBoolean (s,m,c)
   | otherwise = eval (x:s,m,tail c)
    where x = head c