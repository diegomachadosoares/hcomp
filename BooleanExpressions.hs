module BooleanExpressions where

import qualified Data.Map as Map
import Data.Char

import Syntax
import Expressions

evalT :: (E, S, M, C) -> (E, S, M, C)
evalT (e,s,m,c) = (e,head c:s,m,tail c)

evalEq :: (E, S, M, C) -> (E, S, M, C)
evalEq (e,s,m,c) = if s !! 0 == s !! 1 then (e,"tt":drop 2 s,m,tail c) else (e,"ff":drop 2 s,m,tail c)

evalOr :: (E, S, M, C) -> (E, S, M, C)
evalOr (e,s,m,c) = if (s !! 0 == "ff" && s !! 1 == "ff") then (e,"ff":drop 2 s,m,tail c) else (e,"tt":drop 2 s,m,tail c)

-- TODO - Fix negation of "tt"
evalNot :: (E, S, M, C) -> (E, S, M, C)
evalNot (e,s,m,c)
    | head s == "tt" = (e,"ff":drop 1 s,m,tail c)
    | head s == "ff" = (e,"tt":drop 1 s,m,tail c)

evalBoolean :: (E, S, M, C) -> (E, S, M, C)
evalBoolean (e,s,m,c)
    | null c = (e,s,m,c)
    | x == "tt" = evalBoolean (evalT (e,s,m,c))
    | x == "ff" = evalBoolean (evalT (e,s,m,c))
    | x == "=" = evalBoolean (evalEq (e,s,m,c))
    | x == "or" = evalBoolean (evalOr (e,s,m,c))
    | x == "~" = evalBoolean (evalNot (e,s,m,c))
    | x `elem` vars = evalBoolean (evalExp (e,s,m,c))
    | isDigit (head x) = evalBoolean (evalExp (e,s,m,c))
    | otherwise = (e,s,m,c)
    where x = head c