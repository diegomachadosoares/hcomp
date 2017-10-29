module ESMC where

import qualified Data.Map as Map
import Data.Char

import Syntax
import Expressions
import BooleanExpressions
import Commands

eval :: (Env, ValueStack, Memory, ControlStack) -> (ValueStack, Memory, ControlStack)
eval (e,s,m,c)
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
   | otherwise = eval (e,x:s,m,tail c)
    where x = head c