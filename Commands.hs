module Commands where

import qualified Data.Map as Map

evalNil :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalNil (s,m,c) = (s,m,tail c)

evalAttr :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalAttr (s,m,c) = (drop 2 s,Map.insert (s !! 1) (s !! 0) m,tail c)

evalIF :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalIF (s,m,c)
    | x == "tt" = (s,m,tail (takeWhile (/="else") (tail c)))
    | x == "ff" = (s,m,tail (dropWhile (/="else") c))
    where x = head c

whileHelper (s,_,_) = a


evalWhile :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalWhile (s,m,c)
    | x == "while" = evalCMD((c!!1):(c!!3):s,m,(c!!1):"while":c)
    | x == "ff" = (s,m,c)
    where x = head c


evalCMD :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalCMD (s,m,c)
    | null c = (s,m,c)
    | x == "nil" = evalCMD (evalNil (s,m,c))
    | x == ":=" = evalCMD (evalAttr (s,m,c))
    | x == "if" = evalCMD (evalIF (s,m,tail c))
    | x == "while" = evalCMD (evalWhile (s,m,c))
    | otherwise = evalCMD (head c:s,m,tail c)
    where x = head c