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

evalVar :: (E, S, M, C) -> (E, S, M, C)
evalVar (e,s,m,c)
   | isStr x = (e, (convBnd (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar(head c)) e)):s,m,tail c)
   | isLoc x = (e, (convStr (m V.! rBnd (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar $ head c) e))):s, m, tail c)
    where x = (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar (head c)) e)


evalPlus :: (E, S, M, C) -> (E, S, M, C)
evalPlus (e,s,m,c) = (e,ValI (addOp x y):ns,m,c)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalMinus :: (E, S, M, C) -> (E, S, M, C)
evalMinus (e,s,m,c) = (e,ValI (subOp x y):ns,m,c)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalMul :: (E, S, M, C) -> (E, S, M, C)
evalMul (e,s,m,c) = (e,ValI (mulOp x y):ns,m,c)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalT :: (E, S, M, C) -> (E, S, M, C)
evalT (e,s,m,c) = (e,(getVal (head c)):s,m,tail c)

evalEq :: (E, S, M, C) -> (E, S, M, C)
evalEq (e,s,m,c) = if (s !! 0) == (s !! 1) then (e,ValB True:drop 2 s,m,c) else (e,ValB False:drop 2 s,m,c)

evalOr :: (E, S, M, C) -> (E, S, M, C)
evalOr (e,s,m,c) = if (not(rBVal(s !! 0))  && not(rBVal(s !! 1 ))) then (e,ValB False:drop 2 s,m,c) else (e,ValB True:drop 2 s,m,c)

evalNot :: (E, S, M, C) -> (E, S, M, C)
evalNot (e,s,m,c)
    | rBVal(head s) = (e,(ValB False):(tail s),m,c)
    | otherwise = (e,(ValB True):(tail s),m,c)

evalExp :: (E, S, M, C) -> (E, S, M, C)
evalExp (e,s,m,[]) = (e,s,m,[])
evalExp (e,s,m,(ADD):c)= evalExp $ evalPlus (e,s,m,c)
evalExp (e,s,m,(SUB):c)= evalExp $ evalMinus (e,s,m,c)
evalExp (e,s,m,(MUL):c)= evalExp $ evalMul (e,s,m,c)
evalExp (e,s,m,(EQU):c)= evalExp $ evalEq (e,s,m,c)
evalExp (e,s,m,(OR):c)= evalExp $ evalOr (e,s,m,c)
evalExp (e,s,m,(NOT):c)= evalExp $ evalNot (e,s,m,c)
evalExp (e,s,m,(Cexp (Evar a)):c)= evalExp  (evalVar (e,s,m,(Cexp (Evar a)):c))
evalExp (e,s,m,(Cexp (Add a b)):c) = evalExp  (e,s,m,(Cexp a):(Cexp b):(ADD):c)
evalExp (e,s,m,(Cexp (Sub a b)):c) = evalExp (e,s,m,(Cexp a):(Cexp b):(SUB):c)
evalExp (e,s,m,(Cexp (Mul a b)):c) = evalExp (e,s,m,(Cexp a):(Cexp b):(MUL):c)
evalExp (e,s,m,(Cexp (EBool a)):c) = evalExp (evalT (e,s,m,(Cexp (EBool a)):c))
evalExp (e,s,m,(Cexp (Eq a b)):c) = evalExp (e,s,m,(Cexp a):(Cexp b):(EQU):c)
evalExp (e,s,m,(Cexp (Or a b)):c) = evalExp (e,s,m,(Cexp a):(Cexp b):(OR):c)
evalExp (e,s,m,(Cexp (Not a)):c) = evalExp (e,s,m,(Cexp a):(NOT):c)
evalExp (e,s,m,(Cexp (Num a)):c) = evalExp (e,(ValI $ fromIntegral a):s,m,c)
evalExp (e,s,m,c) = (e,s,m,c)
