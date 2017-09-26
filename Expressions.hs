module Expressions where

import qualified Data.Map as Map
import Data.Char

addOp :: Int -> Int -> Int
addOp x y = x + y

subOp :: Int -> Int -> Int
subOp x y = x - y

mulOp :: Int -> Int -> Int
mulOp x y = x * y

vars = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","x","y","z"]

evalVar :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalVar (s,m,c) = ((Map.findWithDefault "0" (head c) m):s, m, tail c)

evalPlus :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalPlus (s,m,c) = (show(addOp x y):ns,m,tail c)
    where   x = read (s !! 1) :: Int
            y = read (s !! 0) :: Int
            ns = drop 2 s

evalMinus :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalMinus (s,m,c) = (show(subOp x y):ns,m,tail c)
    where   x = read (s !! 1) :: Int
            y = read (s !! 0) :: Int
            ns = drop 2 s

evalMul :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalMul (s,m,c) = (show(mulOp x y):ns,m,tail c)
    where   x = read (s !! 1) :: Int
            y = read (s !! 0) :: Int
            ns = drop 2 s

-- TODO eval infix expressions
evalExp :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalExp (s,m,c)
    | null c = (s,m,c)
    | x `elem` vars = evalExp (evalVar (s,m,c))
    | x == "+" = evalExp (evalPlus (s,m,c))
    | x == "-" = evalExp (evalMinus (s,m,c))
    | x == "*" = evalExp (evalMul (s,m,c))
    | isDigit (head x) = evalExp (x:s,m,tail c)
    | otherwise = (s,m,c)
    where x = head c
