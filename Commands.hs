module Commands where

import qualified Data.Map as Map
import Data.List
import Expressions
import BooleanExpressions

evalNil :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalNil (s,m,c) = (s,m,tail c)

evalAttr :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalAttr (s,m,c) = (tail s,Map.insert (head c) (head s) m,tail c)

first (s,_,_) = s

-- TODO Fix error when there is another code block after the 'else' block
evalIF :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalIF (s,m,c)
    | x == "tt" = (s,m,(takeWhile (/="else") (tail (dropWhile (/="then") c) ) ))
    | x == "ff" = (s,m,tail (dropWhile (/="else") c))
    where x = head (first (evalBoolean(s,m,takeWhile (/="then") c)))

evalWhile :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalWhile (s,m,c)
    | x == "tt" = evalCMD(s,m,concat [tail ( (dropWhile (/="do") (takeWhile(/="fimDo") c) ) ),["while"],c])
    | x == "ff" = (s,m,tail (dropWhile (/="fimDo") c))
    where x = head (first (evalBoolean(s,m,takeWhile (/="do") c)))

evalCMD :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalCMD (s,m,c)
    | null c = (s,m,c)
    | x == "nil" = evalCMD (evalNil (s,m,c))
    | x == ":=" = evalCMD (evalAttr (s,m,tail c))
    | x == "if" = evalCMD (evalIF (s,m,tail c))
    | x == "while" = evalCMD (evalWhile (s,m,tail c))
    | otherwise = evalCMD (evalExp (s,m,c))
    where x = head c