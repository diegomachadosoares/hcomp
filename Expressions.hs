module Expressions where

import qualified Data.Map as Map
import Data.Char

import Syntax

addOp :: Int -> Int -> Int
addOp x y = x + y

subOp :: Int -> Int -> Int
subOp x y = x - y

mulOp :: Int -> Int -> Int
mulOp x y = x * y

vars = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","x","y","z"]

evalVar :: (E, S, M, C) -> (E, S, M, C)
evalVar (e,s,m,c)
    | x == "c" = (e,show (e Map.!? (head c)):s,m,tail c)
    | x == "v" = (e, (snd (Map.elemAt (read (snd (Map.findWithDefault ("150","150") (head c) e))) m)):s, m, tail c)
    where x = fst (Map.findWithDefault ("150","150") (head c) e)

evalPlus :: (E, S, M, C) -> (E, S, M, C)
evalPlus (e,s,m,c) = (e,show(addOp x y):ns,m,tail c)
    where   x = read (s !! 1) :: Int
            y = read (s !! 0) :: Int
            ns = drop 2 s

evalMinus :: (E, S, M, C) -> (E, S, M, C)
evalMinus (e,s,m,c) = (e,show(subOp x y):ns,m,tail c)
    where   x = read (s !! 1) :: Int
            y = read (s !! 0) :: Int
            ns = drop 2 s

evalMul :: (E, S, M, C) -> (E, S, M, C)
evalMul (e,s,m,c) = (e,show(mulOp x y):ns,m,tail c)
    where   x = read (s !! 1) :: Int
            y = read (s !! 0) :: Int
            ns = drop 2 s

-- TODO eval infix expressions
evalExp :: (E, S, M, C) -> (E, S, M, C)
evalExp (e,s,m,c)
    | null c = (e,s,m,c)
    | x `elem` vars = evalExp (evalVar (e,s,m,c))
    | x == "+" = evalExp (evalPlus (e,s,m,c))
    | x == "-" = evalExp (evalMinus (e,s,m,c))
    | x == "*" = evalExp (evalMul (e,s,m,c))
    | isDigit (head x) = evalExp (e,x:s,m,tail c)
    | otherwise = (e,s,m,c)
    where x = head c
