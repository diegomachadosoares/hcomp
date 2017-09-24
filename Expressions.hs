module Expressions where

addOp :: Int -> Int -> Int
addOp x y = x + y

subOp :: Int -> Int -> Int
subOp x y = x - y

mulOp :: Int -> Int -> Int
mulOp x y = x * y

evalPlus :: ([String],[String],[String]) -> ([String],[String],[String])
evalPlus (a,b,c) = (show(addOp x y):na,b,tail c)
    where   x = read (a !! 0) :: Int
            y = read (a !! 1) :: Int
            na = drop 2 a

evalMinus :: ([String],[String],[String]) -> ([String],[String],[String])
evalMinus (a,b,c) = (show(subOp x y):na,b,tail c)
    where   x = read (a !! 0) :: Int
            y = read (a !! 1) :: Int
            na = drop 2 a

evalMul :: ([String],[String],[String]) -> ([String],[String],[String])
evalMul (a,b,c) = (show(mulOp x y):na,b,tail c)
    where   x = read (a !! 0) :: Int
            y = read (a !! 1) :: Int
            na = drop 2 a

evalExp :: ([String],[String],[String]) -> ([String],[String],[String])
evalExp (a,b,c)
    | x == ";" = (a,b,tail c)
    | x == "+" = evalExp (evalPlus (a,b,c))
    | x == "-" = evalExp (evalMinus (a,b,c))
    | x == "*" = evalExp (evalMul (a,b,c))
    | otherwise = evalExp (x:a,b,tail c)
    where x = head c
