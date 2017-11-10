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
evalAttr (e,s,m,(Ccom (Attr a exp)):c)
    | x == BndLoc (Loc (-1)) = (e,s,m,c)
    | otherwise = (e, tail s,(m V.// [(rBnd x, convValStr(head s))]),c)
    where x = Map.findWithDefault (BndLoc $ Loc (-1)) a e

evalIF :: (E, S, M, C) -> (E, S, M, C)
evalIF (e,s,m,(Ccom (If exp a b)):c)
    | x = (e,filterS (evalCMD (e,s,m,a)),filterM (evalCMD (e,s,m,a)),c)
    | otherwise = (e,filterS (evalCMD (e,s,m,b)),filterM (evalCMD (e,s,m,b)),c)
    where x = rBVal(head (filterS (evalExp(e,s,m,exp))))
         -- v = filterS (evalCMD (e,s,m,a))
         -- y = filterM (evalCMD (e,s,m,a))
         -- k = filterS (evalCMD (e,s,m,b))
         -- z = filterM (evalCMD (e,s,m,b))

evalWhile :: (E, S, M, C) -> (E, S, M, C)
evalWhile (e,s,m,Ccom (While exp a):c)
    | x = evalCMD(e,s,m,c)
    | otherwise = (e,v,y,tail c)
    -- TODO REVISAR!
    where x = rBVal (head (filterS (evalExp(e,s,m,exp))))
          v = filterS (evalGen(e,s,m,a))
          y = filterM (evalGen(e,s,m,a))

evalCMD :: (E, S, M, C) -> (E, S, M, C)
evalCMD (e,s,m,[]) = (e,s,m,[])
evalCMD (e,s,m,(Ccom Nill):c) = evalCMD (evalNil (e,s,m,c))
evalCMD (e,s,m,(Ccom (If exp a b)):c) = evalCMD (evalIF (e,s,m,(Ccom (If exp a b)):c))
evalCMD (e,s,m,(Ccom (While exp a)):c) = evalCMD (evalIF (e,s,m,(Ccom (While exp a)):c))
evalCMD (e,s,m,(Ccom (Attr a exp)):c) = evalCMD (evalIF (e,s,m,(Ccom (Attr a exp)):c))
evalCMD (e,s,m,c) = (e,s,m,c)

evalGen :: (E, S, M, C) -> (E, S, M, C)
evalGen (e,s,m,c) = (e,s,m,c)