module Commands where

import qualified Data.Map as Map

evalNil :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalNil (a,b,c) = (a,b,tail c)

evalAttr :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalAttr (a,b,c) = (drop 2 a,Map.insert (a !! 0) (a !! 1) b,tail c)

evalIF :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalIF (a,b,c)
    | x == "if" = evalIF (a,b,tail c)
    | x == "then" = evalIF (head c:a,b,tail c)
    | x == "else" = evalIF (head c:a,b,tail c)
    where x = head c

evalCMD :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalCMD (a,b,c)
    | null c = (a,b,c)
    | x == "nil" = evalCMD (evalNil (a,b,c))
    | x == ":=" = evalCMD (evalAttr (a,b,c))
    -- | x == ";" = evalCMD (tail a,b,head a:tail c)
    | x == "if" = evalCMD (evalIF (a,b,c))
    | otherwise = evalCMD (head c:a,b,tail c)
    where x = head c