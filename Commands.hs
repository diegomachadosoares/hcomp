module Commands where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.List

import Syntax
import HelperTools
import Expressions
import Declaration

evalCMD :: (E, S, M, C) -> (E, S, M, C)

evalCMD (e,s,m,[]) = (e,s,m,[])

evalCMD (e,s,m,(Ccom Nill):c) = evalCMD (e,s,m,c)

evalCMD (e,s,m,(Ccom (If exp a b)):c)
    | x = (e,filterS (evalCMD (e,s,m,a)),filterM (evalCMD (e,s,m,a)),c)
    | otherwise = (e,filterS (evalCMD (e,s,m,b)),filterM (evalCMD (e,s,m,b)),c)
    where x = rBVal(head (filterS (evalExp(e,s,m,exp))))

evalCMD (e,s,m,(Ccom (While exp a)):c)
    | x  = (e,filterS (evalCMD(e,s,m,a)),filterM (evalCMD(e,s,m,a)),(Ccom (While exp a)):c)
    | otherwise = evalCMD(e,s,m,c)
    where x = rBVal (head (filterS (evalExp(e,s,m,exp))))

evalCMD (e,s,m,(Ccom (Attr a exp)):c)
    | x == BndLoc (Loc (-1)) = (e,s,m,c)
    | otherwise = (e,s,(m V.// [(rBnd x, convValStr(head (filterS (evalExp (e,s,m,exp)))))]),c)
    where x = Map.findWithDefault (BndLoc $ Loc (-1)) a e

evalCMD (e,s,m,(Ccom (Var a b exp)):c) = evalDec(e,s,m,(Ccom (Var a b exp)):c)

evalCMD (e,s,m,c) = (e,s,m,c)
