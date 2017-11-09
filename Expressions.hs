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

vars = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","x","y","z"]

evalVar :: (E, S, M, C) -> (E, S, M, C)
evalVar (e,s,m,c)
   | isStr x = (e, (convBnd (Map.findWithDefault (BndLoc $ Loc 0) (getVar(head c)) e)):s,m,tail c)
   | isLoc x = (e, (convStr (m V.! rBnd (Map.findWithDefault (BndLoc $ Loc 0) (getVar $ head c) e))):s, m, tail c)
    where x = (Map.findWithDefault (BndLoc $ Loc 0) (getVar (head c)) e)


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
evalT (e,s,m,c) = (e,(getVal (head c)):s,m,c)

evalEq :: (E, S, M, C) -> (E, S, M, C)
evalEq (e,s,m,c) = if rBVal (s !! 0) == rBVal (s !! 1) then (e,ValB True:drop 2 s,m,tail c) else (e,ValB False:drop 2 s,m,tail c)

evalOr :: (E, S, M, C) -> (E, S, M, C)
evalOr (e,s,m,c) = if (not(rBVal(s !! 0))  && not(rBVal(s !! 1 ))) then (e,ValB False:drop 2 s,m,tail c) else (e,ValB True:drop 2 s,m,tail c)

-- TODO - Fix negation of "tt"
evalNot :: (E, S, M, C) -> (E, S, M, C)
evalNot (e,s,m,c)
    | rBVal(head s) = (e,ValB False:drop 1 s,m,tail c)
    | otherwise = (e,ValB True:drop 1 s,m,tail c)

-- TODO eval infix expressions
evalExp :: (E, S, M, C) -> (E, S, M, C)
evalExp (e,s,m,[]) = (e,s,m,[])
evalExp (e,s,m,(Cvar a):c)= evalExp (evalVar (e,s,m,(Cvar a):c))
evalExp (e,s,m,(Cexp Add):c) = evalExp (evalPlus (e,s,m,c))
evalExp (e,s,m,(Cexp Sub):cc) = evalExp (evalMinus (e,s,m,c))
evalExp (e,s,m,(Cexp Mul):cc) = evalExp (evalMul (e,s,m,c))
evalExp (e,s,m,(Cexp ):cc) = evalExp (evalT (e,s,m,c))
evalExp (e,s,m,(Cexp ()):cc) = evalExp (evalT (e,s,m,c))
evalExp (e,s,m,(Cexp ()):cc) = evalExp (evalEq (e,s,m,c))
evalExp (e,s,m,(Cexp ()):cc) = evalExp (evalOr (e,s,m,c))
evalExp (e,s,m,(Cexp ()):cc) = evalExp (evalNot (e,s,m,c))
evalExp (e,s,m,(Cexp (Num a)):c) = evalExp (e,(ValI a):s,m,c)
evalExp (e,s,m,c) = (e,s,m,c)
