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
evalVar (a,b,c) = ((Map.findWithDefault "0" (head c) b):a, b, tail c)

evalPlus :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalPlus (a,b,c) = (show(addOp x y):na,b,tail c)
    where   x = read (a !! 1) :: Int
            y = read (a !! 0) :: Int
            na = drop 2 a

evalMinus :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalMinus (a,b,c) = (show(subOp x y):na,b,tail c)
    where   x = read (a !! 1) :: Int
            y = read (a !! 0) :: Int
            na = drop 2 a

evalMul :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalMul (a,b,c) = (show(mulOp x y):na,b,tail c)
    where   x = read (a !! 1) :: Int
            y = read (a !! 0) :: Int
            na = drop 2 a

-- TODO eval infix expressions
evalExp :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
evalExp (a,b,c)
    | null c = (a,b,c)
    -- TODO use any letter as variable!
    | x `elem` vars = evalExp (evalVar (a,b,c))
    | x == "+" = evalExp (evalPlus (a,b,c))
    | x == "-" = evalExp (evalMinus (a,b,c))
    | x == "*" = evalExp (evalMul (a,b,c))
    | isDigit (head x) = evalExp (x:a,b,tail c)
    where x = head c
