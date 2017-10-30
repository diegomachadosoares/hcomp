module Commands where

import qualified Data.Map as Map
import Data.List

import Syntax
import Expressions
import BooleanExpressions

evalNil :: (E, S, M, C) -> (E, S, M, C)
evalNil (e,s,m,c) = (e,s,m,tail c)

evalAttr :: (E, S, M, C) -> (E, S, M, C)
evalAttr (e,s,m,c) = (e,tail s,Map.insert (head c) (head s) m,tail c)

first (_,s,_,_) = s

-- TODO test Fixed error when there is another code block after the 'else' block
evalIF :: (E, S, M, C) -> (E, S, M, C)
evalIF (e,s,m,c)
    | x == "tt" = (e,s,m,concat [(takeWhile (/="else") (tail (dropWhile (/="then") c) ) ), (tail (dropWhile (/="fimElse") c))])
    | x == "ff" = (e,s,m,tail (dropWhile (/="else") c))
    where x = head (first (evalBoolean(e,s,m,takeWhile (/="then") c)))

evalWhile :: (E, S, M, C) -> (E, S, M, C)
evalWhile (e,s,m,c)
    | x == "tt" = evalCMD(e,s,m,concat [tail ( (dropWhile (/="do") (takeWhile(/="fimDo") c) ) ),["while"],c])
    | x == "ff" = (e,s,m,tail (dropWhile (/="fimDo") c))
    where x = head (first (evalBoolean(e,s,m,takeWhile (/="do") c)))

evalCMD :: (E, S, M, C) -> (E, S, M, C)
evalCMD (e,s,m,c)
    | null c = (e,s,m,c)
    | x == "nil" = evalCMD (evalNil (e,s,m,c))
    | x == ":=" = evalCMD (evalAttr (e,s,m,tail c))
    | x == "if" = evalCMD (evalIF (e,s,m,tail c))
    | x == "while" = evalCMD (evalWhile (e,s,m,tail c))
    | x == "fimElse" = (e,s,m,tail c)
    | otherwise = evalCMD (evalExp (e,s,m,c))
    where x = head c