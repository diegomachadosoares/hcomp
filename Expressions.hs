module Expressions where

import qualified Data.Map as Map
import qualified Data.Vector as V
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
    | x == "c" = (e, snd (Map.findWithDefault ("-1","-1") (head c) e):s,m,tail c)
    | x == "v" = (e, (m V.! (read (snd (Map.findWithDefault ("-1","-1") (head c) e)))):s, m, tail c)
    -- Error! Variable not found in memory!
    | x == "-1" = (e,s,m,c)
    where x = fst (Map.findWithDefault ("-1","-1") (head c) e)


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

evalT :: (E, S, M, C) -> (E, S, M, C)
evalT (e,s,m,c) = (e,head c:s,m,tail c)

evalEq :: (E, S, M, C) -> (E, S, M, C)
evalEq (e,s,m,c) = if s !! 0 == s !! 1 then (e,"tt":drop 2 s,m,tail c) else (e,"ff":drop 2 s,m,tail c)

evalOr :: (E, S, M, C) -> (E, S, M, C)
evalOr (e,s,m,c) = if (s !! 0 == "ff" && s !! 1 == "ff") then (e,"ff":drop 2 s,m,tail c) else (e,"tt":drop 2 s,m,tail c)

-- TODO - Fix negation of "tt"
evalNot :: (E, S, M, C) -> (E, S, M, C)
evalNot (e,s,m,c)
    | head s == "tt" = (e,"ff":drop 1 s,m,tail c)
    | head s == "ff" = (e,"tt":drop 1 s,m,tail c)

-- TODO eval infix expressions
evalExp :: (E, S, M, C) -> (E, S, M, C)
evalExp (e,s,m,c)
    | null c = (e,s,m,c)
    | x `elem` vars = evalExp (evalVar (e,s,m,c))
    | x == "+" = evalExp (evalPlus (e,s,m,c))
    | x == "-" = evalExp (evalMinus (e,s,m,c))
    | x == "*" = evalExp (evalMul (e,s,m,c))
    | x == "tt" = evalExp (evalT (e,s,m,c))
    | x == "ff" = evalExp (evalT (e,s,m,c))
    | x == "=" = evalExp (evalEq (e,s,m,c))
    | x == "or" = evalExp (evalOr (e,s,m,c))
    | x == "~" = evalExp (evalNot (e,s,m,c))
    | isDigit (head x) = evalExp (e,x:s,m,tail c)
    | otherwise = (e,s,m,c)
    where x = head c