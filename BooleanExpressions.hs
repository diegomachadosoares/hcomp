module BooleanExpressions where

import qualified Data.Map as Map

evalT :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String, [String])
evalT (a,b,c) = (head c:a,b,tail c)

evalEq :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String, [String])
evalEq (a,b,c) = if a !! 0 == a !! 1 then ("tt":drop 2 a,b,tail c) else ("ff":drop 2 a,b,tail c)

evalOr :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String, [String])
evalOr (a,b,c) = if (a !! 0 == "ff" && a !! 1 == "ff") then ("ff":drop 2 a,b,tail c) else ("tt":drop 2 a,b,tail c)

-- TODO - Fix negation of "tt"
evalTil :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String, [String])
evalTil (a,b,c) = if (head a == "tt") then ("ff":drop 1 a,b,c) else ("tt":drop 1 a,b,tail c)

evalBoolean :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String, [String])
evalBoolean (a,b,c)
    | null c = (a,b,c)
    | x == "tt" = evalBoolean (evalT (a,b,c))
    | x == "ff" = evalBoolean (evalT (a,b,c))
    | x == "=" = evalBoolean (evalEq (a,b,c))
    | x == "or" = evalBoolean (evalOr (a,b,c))
    | x == "~" = evalBoolean (evalTil(a,b,c))
    -- TODO improve edge case!
    | otherwise = evalBoolean (head c:a,b,tail c)
    where x = head c