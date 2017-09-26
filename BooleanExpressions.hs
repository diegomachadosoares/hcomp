module BooleanExpressions where

import qualified Data.Map as Map
import Expressions

evalT :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String, [String])
evalT (s,m,c) = (head c:s,m,tail c)

evalEq :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String, [String])
evalEq (s,m,c) = if s !! 0 == s !! 1 then ("tt":drop 2 s,m,tail c) else ("ff":drop 2 s,m,tail c)

evalOr :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String, [String])
evalOr (s,m,c) = if (s !! 0 == "ff" && s !! 1 == "ff") then ("ff":drop 2 s,m,tail c) else ("tt":drop 2 s,m,tail c)

-- TODO - Fix negation of "tt"
evalNot :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String, [String])
evalNot (s,m,c)
    | head s == "tt" = ("ff":drop 1 s,m,tail c)
    | head s == "ff" = ("tt":drop 1 s,m,tail c)

evalBoolean :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String, [String])
evalBoolean (s,m,c)
    | null c = (s,m,c)
    | x == "tt" = evalBoolean (evalT (s,m,c))
    | x == "ff" = evalBoolean (evalT (s,m,c))
    | x == "=" = evalBoolean (evalEq (s,m,c))
    | x == "or" = evalBoolean (evalOr (s,m,c))
    | x == "~" = evalBoolean (evalNot (s,m,c))
    | x `elem` vars = evalBoolean (evalExp (s,m,c))
    where x = head c