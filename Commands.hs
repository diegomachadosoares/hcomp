module Commands where

import qualified Data.Map as Map

evalNil :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalNil (a,b,c) = (a,b,tail c)

evalAttr :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalAttr (a,b,c) = (drop 2 a,Map.insert (a !! 1) (a !! 0) b,tail c)

evalIF :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalIF (a,b,c)
    | x == "tt" = (a,b,tail (takeWhile (/="else") (tail c)))
    | x == "ff" = (a,b,tail (dropWhile (/="else") c))
    where x = head c

whileHelper (a,_,_) = a


evalWhile :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalWhile (a,b,c)
    | x == "while" = evalCMD((c!!1):(c!!3):a,b,(c!!1):"while":c)
    | x == "ff" = (a,b,c)
    where x = head c


evalCMD :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalCMD (a,b,c)
    | null c = (a,b,c)
    | x == "nil" = evalCMD (evalNil (a,b,c))
    | x == ":=" = evalCMD (evalAttr (a,b,c))
    | x == "if" = evalCMD (evalIF (a,b,tail c))
    | x == "while" = evalCMD (evalWhile (a,b,c))
    | otherwise = evalCMD (head c:a,b,tail c)
    where x = head c