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

evalVar :: (E, S, M, Cexp) -> (E, S, M, Cexp)
evalVar (e,s,m,c)
   | isStr x = (e, (convBnd (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar(head c)) e)):s,m,tail c)
   | isLoc x = (e, (convStr (m V.! rBnd (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar $ head c) e))):s, m, tail c)
    where x = (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar (head c)) e)


evalPlus :: (E, S, M, Cexp) -> (E, S, M, Cexp)
evalPlus (e,s,m,c) = (e,ValI (addOp x y):ns,m,c)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalMinus :: (E, S, M, Cexp) -> (E, S, M, Cexp)
evalMinus (e,s,m,c) = (e,ValI (subOp x y):ns,m,c)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalMul :: (E, S, M, Cexp) -> (E, S, M, Cexp)
evalMul (e,s,m,c) = (e,ValI (mulOp x y):ns,m,c)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalT :: (E, S, M, Cexp) -> (E, S, M, Cexp)
evalT (e,s,m,c) = (e,(getVal (head c)):s,m,tail c)

evalEq :: (E, S, M, Cexp) -> (E, S, M, Cexp)
evalEq (e,s,m,c) = if rBVal (s !! 0) == rBVal (s !! 1) then (e,ValB True:drop 2 s,m,tail c) else (e,ValB False:drop 2 s,m,tail c)

evalOr :: (E, S, M, Cexp) -> (E, S, M, Cexp)
evalOr (e,s,m,c) = if (not(rBVal(s !! 0))  && not(rBVal(s !! 1 ))) then (e,ValB False:drop 2 s,m,tail c) else (e,ValB True:drop 2 s,m,tail c)

evalNot :: (E, S, M, Cexp) -> (E, S, M, Cexp)
evalNot (e,s,m,c)
    | rBVal(head s) = (e,ValB False:drop 1 s,m,tail c)
    | otherwise = (e,ValB True:drop 1 s,m,tail c)

evalExp :: (E, S, M, Cexp) -> (E, S, M, Cexp)
evalExp (e,s,m,[]) = (e,s,m,[])
evalExp (e,s,m,(Evar a):c)= evalExp (evalVar (e,s,m,(Evar a):c))
evalExp (e,s,m,(Add):c) = evalExp (evalPlus (e,s,m,c))
evalExp (e,s,m,(Sub):c) = evalExp (evalMinus (e,s,m,c))
evalExp (e,s,m,(Mul):c) = evalExp (evalMul (e,s,m,c))
evalExp (e,s,m,(EBool a):c) = evalExp (evalT (e,s,m,(EBool a):c))
evalExp (e,s,m,(Eq):c) = evalExp (evalEq (e,s,m,c))
evalExp (e,s,m,(Or):c) = evalExp (evalOr (e,s,m,c))
evalExp (e,s,m,(Not):c) = evalExp (evalNot (e,s,m,c))
evalExp (e,s,m,(Num a):c) = evalExp (e,(ValI a):s,m,c)
evalExp (e,s,m,c) = (e,s,m,c)
