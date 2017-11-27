module Expressions where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Char

import Syntax
import HelperTools

addOp :: Int -> Int -> Int
addOp x y = x + y

subOp :: Int -> Int -> Int
subOp x y = x - y

mulOp :: Int -> Int -> Int
mulOp x y = x * y

evalVar :: (E, S, M, C, O) -> (E, S, M, C, O)
evalVar (e,s,m,c,o)
   | isStr x = (e, (convBnd (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar(head c)) e)):s,m,tail c,o)
   | isLoc x = (e, (convStr (m V.! rBnd (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar $ head c) e))):s, m, tail c,o)
    where x = (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar (head c)) e)


evalPlus :: (E, S, M, C, O) -> (E, S, M, C, O)
evalPlus (e,s,m,c,o) = (e,ValI (addOp x y):ns,m,c,o)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalMinus :: (E, S, M, C, O) -> (E, S, M, C, O)
evalMinus (e,s,m,c,o) = (e,ValI (subOp x y):ns,m,c,o)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalMul :: (E, S, M, C, O) -> (E, S, M, C, O)
evalMul (e,s,m,c,o) = (e,ValI (mulOp x y):ns,m,c,o)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalT :: (E, S, M, C, O) -> (E, S, M, C, O)
evalT (e,s,m,c,o) = (e,(getVal (head c)):s,m,tail c,o)

evalEq :: (E, S, M, C, O) -> (E, S, M, C, O)
evalEq (e,s,m,c,o) = if (s !! 0) == (s !! 1) then (e,ValB True:drop 2 s,m,c,o) else (e,ValB False:drop 2 s,m,c,o)

evalOr :: (E, S, M, C, O) -> (E, S, M, C, O)
evalOr (e,s,m,c,o) = if (not(rBVal(s !! 0))  && not(rBVal(s !! 1 ))) then (e,ValB False:drop 2 s,m,c,o) else (e,ValB True:drop 2 s,m,c,o)

evalNot :: (E, S, M, C, O) -> (E, S, M, C, O)
evalNot (e,s,m,c,o)
    | rBVal(head s) = (e,(ValB False):(tail s),m,c,o)
    | otherwise = (e,(ValB True):(tail s),m,c,o)

evalExp :: (E, S, M, C, O) -> (E, S, M, C, O)
evalExp (e,s,m,[],o) = (e,s,m,[],o)
evalExp (e,s,m,(ADD):c,o)= evalExp $ evalPlus (e,s,m,c,o)
evalExp (e,s,m,(SUB):c,o)= evalExp $ evalMinus (e,s,m,c,o)
evalExp (e,s,m,(MUL):c,o)= evalExp $ evalMul (e,s,m,c,o)
evalExp (e,s,m,(EQU):c,o)= evalExp $ evalEq (e,s,m,c,o)
evalExp (e,s,m,(OR):c,o)= evalExp $ evalOr (e,s,m,c,o)
evalExp (e,s,m,(NOT):c,o)= evalExp $ evalNot (e,s,m,c,o)
evalExp (e,s,m,(Cexp (Evar a)):c,o)= evalExp  (evalVar (e,s,m,(Cexp (Evar a)):c,o))
evalExp (e,s,m,(Cexp (Add a b)):c,o) = evalExp  (e,s,m,(Cexp a):(Cexp b):(ADD):c,o)
evalExp (e,s,m,(Cexp (Sub a b)):c,o) = evalExp (e,s,m,(Cexp a):(Cexp b):(SUB):c,o)
evalExp (e,s,m,(Cexp (Mul a b)):c,o) = evalExp (e,s,m,(Cexp a):(Cexp b):(MUL):c,o)
evalExp (e,s,m,(Cexp (EBool a)):c,o) = evalExp (evalT (e,s,m,(Cexp (EBool a)):c,o))
evalExp (e,s,m,(Cexp (Eq a b)):c,o) = evalExp (e,s,m,(Cexp a):(Cexp b):(EQU):c,o)
evalExp (e,s,m,(Cexp (Or a b)):c,o) = evalExp (e,s,m,(Cexp a):(Cexp b):(OR):c,o)
evalExp (e,s,m,(Cexp (Not a)):c,o) = evalExp (e,s,m,(Cexp a):(NOT):c,o)
evalExp (e,s,m,(Cexp (Num a)):c,o) = evalExp (e,(ValI $ fromIntegral a):s,m,c,o)
evalExp (e,s,m,c,o) = (e,s,m,c,o)
