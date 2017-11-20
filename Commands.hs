module Commands where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.List
import Data.Char

import Syntax
import HelperTools
import Expressions
import Declaration

evalCMD :: (E, S, M, C, O) -> (E, S, M, C, O)

evalCMD (e,s,m,[],o) = (e,s,m,[],o)

evalCMD (e,s,m,(Ccom Nill):c,o) = evalCMD (e,s,m,c,0)

evalCMD (e,s,m,(Ccom (If exp a b)):c,o)
    | x = evalCMD (e,filterS (evalCMD (e,s,m,a)),filterM (evalCMD (e,s,m,a)),c,o)
    | otherwise = evalCMD (e,filterS (evalCMD (e,s,m,b)),filterM (evalCMD (e,s,m,b)),c,o)
    where x = rBVal(head (filterS (evalExp(e,s,m,[(Cexp exp)],o))))

evalCMD (e,s,m,(Ccom (While exp a)):c)
    | x  = evalCMD(e,filterS (evalCMD(e,s,m,a,o)),filterM (evalCMD(e,s,m,a,o)),(Ccom (While exp a)):c,o)
    | otherwise = evalCMD(e,s,m,c,o)
    where x = rBVal (head (filterS (evalExp(e,s,m,[(Cexp exp)],o))))

evalCMD (e,s,m,(Ccom (Attr a exp)):c,o)
    | x == BndLoc (Loc (-1)) = (e,s,m,c,o)
    | otherwise = evalCMD (e,s,(m V.// [(rBnd x, convValStr(head (filterS (evalExp (e,s,m,[(Cexp exp)],o)))))]),c,o)
    where x = Map.findWithDefault (BndLoc $ Loc (-1)) a e

evalCMD (e,s,m,(Ccom (Var a b exp)):c,o) = evalCMD (evalDec(e,s,m,(Ccom (Var a b exp)):c,o))
evalCMD (e,s,m,(Ccom (Const a b exp)):c,o) = evalCMD (evalDec(e,s,m,(Ccom (Const a b exp)):c,o))
evalCMD (e,s,m,(Ccom (Sequence a b)):c,o) = evalCMD (e,s,m,(Ccom a):(Ccom b):c,o)
evalCMD (e,s,m,(Ccom (Print exp)):c,o) = evalCMD (e,s,m,c,head(filterS (evalExp(e,s,m,[(Cexp exp)],o))):o)
evalCMD (e,s,m,c,o) = (e,s,m,c,o)