module Commands where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.List

import Syntax
import HelperTools
import Expressions

evalNil :: (E, S, M, C) -> (E, S, M, C)
evalNil (e,s,m,c) = (e,s,m,c)

evalAttr :: (E, S, M, C) -> (E, S, M, C)
evalAttr (e,s,m,(Attr a exp):c)
    | x == Loc -1 = (e,s,m,c)
    | otherwise = (e, tail s,(m V.// [(x, (head s))]),c)
    where x = Map.findWithDefault (BndLoc $ Loc (-1)) a e

evalIF :: (E, S, M, C) -> (E, S, M, C)
evalIF (e,s,m,c)
    | x == "tt" = (e,s,m,concat [(takeWhile (/="else") (tail (dropWhile (/="then") c) ) ), (tail (dropWhile (/="fimElse") c))])
    | x == "ff" = (e,s,m,tail (dropWhile (/="else") c))
    where x = head (filterS (evalExp(e,s,m,takeWhile (/="then") c)))

evalWhile :: (E, S, M, C) -> (E, S, M, C)
evalWhile (e,s,m,c)
    | x == "tt" = evalCMD(e,s,m,concat [tail ( (dropWhile (/="do") (takeWhile(/="fimDo") c) ) ),["while"],c])
    | x == "ff" = (e,s,m,tail (dropWhile (/="fimDo") c))
    where x = head (filterS (evalExp(e,s,m,takeWhile (/="do") c)))

evalCMD :: (E, S, M, C) -> (E, S, M, C)
evalCMD (e,s,m,[]) = (e,s,m,[])
evalCMD (e,s,m,(Ccom Nill):c) = evalCMD (evalNil (e,s,m,c))
evalCMD (e,s,m,(If exp a b):c) = evalCMD (evalIF (e,s,m,(If exp a b):c))
evalCMD (e,s,m,(While exp a):c) = evalCMD (evalIF (e,s,m,(While exp a):c))
evalCMD (e,s,m,(Attr a exp):c) = evalCMD (evalIF (e,s,m,(Attr a exp):c))
evalCMD (e,s,m,c) = (e,s,m,c)
