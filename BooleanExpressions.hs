module BooleanExpressions where

evalT :: ([String],[(String,Int)],[String]) -> ([String],[(String,Int)],[String])
evalT (a,b,c) = (head c:a,b,tail c)

evalEq :: ([String],[(String,Int)],[String]) -> ([String],[(String,Int)],[String])
evalEq (a,b,c) = if a !! 0 == a !! 1 then ("tt":a,b,tail c) else ("ff":a,b,tail c)

evalOr :: ([String],[(String,Int)],[String]) -> ([String],[(String,Int)],[String])
evalOr (a,b,c) = if (a !! 0 == "ff" && a !! 1 == "ff") then ("ff":drop 2 a,b,tail c) else ("tt":drop 2 a,b,tail c)

evalTil :: ([String],[(String,Int)],[String]) -> ([String],[(String,Int)],[String])
evalTil (a,b,c) = if (head a == "tt") then ("ff":drop 2 a,b,tail c) else ("tt":drop 2 a,b,tail c)

evalBoolean :: ([String],[(String,Int)],[String]) -> ([String],[(String,Int)],[String])
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