module Commands where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.List
import Data.Char

import Syntax
import HelperTools
import Expressions

evalCMD :: (E, S, M, C, O) -> (E, S, M, C, O)

evalCMD (e,s,m,[],o) = (e,s,m,[],o)

evalCMD (e,s,m,(Ccom Nill):c,o) = evalCMD (e,s,m,c,o)

evalCMD (e,s,m,(Ccom (If exp a b)):c,o)
    | x = evalCMD (e,filterS (evalCMD (e,s,m,a,o)),filterM (evalCMD (e,s,m,a,o)),c,o)
    | otherwise = evalCMD (e,filterS (evalCMD (e,s,m,b,o)),filterM (evalCMD (e,s,m,b,o)),c,o)
    where x = rBVal(head (filterS (evalExp(e,s,m,[(Cexp exp)],o))))

evalCMD (e,s,m,(Ccom (While exp a)):c,o)
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

evalDecExp :: (E, S, M, C, O) -> (E, S, M, C, O)
evalDecExp (e,s,m,[],o) = (e,s,m,[],o)
evalDecExp (e,s,m,(Cexp (IfExp a b d)):c,o)= evalExpIF (e,s,m,(Cexp (IfExp a b d)):c,o)
evalDecExp (e,s,m,c,o)= evalExp(e,s,m,c,o)

evalExpIF :: (E, S, M, C, O) -> (E, S, M, C, O)
evalExpIF (e,s,m,(Cexp (IfExp a b d)):c,o)
    | x = (e,v,m,tail c,o)
    | otherwise = evalDecExp(e,s,m,tail c,o)
    where x = rBVal (head (filterS (evalExp(e,s,m,[Cexp a],o))))
          v = filterS (evalExp(e,s,m,[Cexp b],o))

evalDec :: (E, S, M, C, O) -> (E, S, M, C, O)
evalDec (e,s,m,[],o) = (e,s,m,[],o)
evalDec (e,s,m,(Ccom (Var a b d)):c,o) = free (evalCMD (((Map.insert a (BndLoc (Loc ( V.length m)))) e),s,m V.++ (V.singleton (convValStr(head(filterS (evalDecExp(e,s,m,[Cexp d],o)))))),c,o))
evalDec (e,s,m,(Ccom (Const a b d)):c,o) = (Map.insert a (convValBnd(head (filterS (evalDecExp(e,s,m,[Cexp d],o))))) e,s,m,c,o)
evalDec (e,s,m,(Ccom (ProcR id f bl)):c,o) = (Map.insert id (BndAbs (f,(Ccom (ProcR id f bl)):bl)) e,s,m,c,o)
evalDec (e,s,m,c,o) = (e,s,m,c,o)

free (e,s,m,c,o) = (e,s,m,c,o)