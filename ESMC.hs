module ESMC where

import qualified Data.Map as Map
import Data.Char

import Syntax
import Expressions
import Commands
import Declaration

eval :: (E, S, M, C) -> (E, S, M, C)
eval (e,s,m,c)
   | null c = (e,s,m,c)
   | x `elem` vars = eval (evalExp (e,s,m,c))
   | x == "+" = eval (evalExp (e,s,m,c))
   | x == "-" = eval (evalExp (e,s,m,c))
   | x == "*" = eval (evalExp (e,s,m,c))
   | x == "tt" = eval (evalExp (e,s,m,c))
   | x == "ff" = eval (evalExp (e,s,m,c))
   | x == "=" = eval (evalExp (e,s,m,c))
   | x == "or" = eval (evalExp (e,s,m,c))
   | x == "~" = eval (evalExp (e,s,m,c))
   | x == "nil" = eval (evalCMD (e,s,m,c))
   | x == ":=" = eval (evalCMD (e,s,m,c))
   | x == "if" = eval (evalCMD (e,s,m,c))
   | x == "while" = eval (evalCMD (e,s,m,c))
   | x == "const" = eval (evalDec (e,s,m,c))
   | x == "var" = eval (evalDec (e,s,m,c))
   | otherwise = eval (eval (e,x:s,m,tail c))
    where x = head c