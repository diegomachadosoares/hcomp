module SMC where

import qualified Data.Map as Map
import Data.Char

import Expressions
import BooleanExpressions
import Commands

eval :: ([String], Map.Map String String, [String]) -> ([String], Map.Map String String, [String])
eval (a,b,c)
   | null c = (a,b,c)
   | x == "v" = evalExp (a,b,c)
   | x == "+" = evalExp (a,b,c)
   | x == "-" = evalExp (a,b,c)
   | x == "*" = evalExp (a,b,c)
   | x == "nil" = evalCMD (a,b,c)
   | x == ":=" = evalCMD (a,b,c)
   | x == "if" = evalCMD (a,b,c)
   | x == "tt" = evalBoolean (a,b,c)
   | x == "ff" = evalBoolean (a,b,c)
   | x == "=" = evalBoolean (a,b,c)
   | x == "or" = evalBoolean (a,b,c)
   | x == "~" = evalBoolean (a,b,c)
   | otherwise = eval (x:a,b,tail c)
    where x = head c