module Expressions where

addOp :: Int -> Int -> Int
addOp x y = x + y

subOp :: Int -> Int -> Int
subOp x y = x - y

mulOp :: Int -> Int -> Int
mulOp x y = x * y

findKey :: String -> [(String,Int)] -> String
findKey key [] = "nil"
findKey key ((k,v):xs) = if key == k
                         then show(v)
                         else findKey key xs

evalVar :: ([String],[(String,Int)],[String]) -> ([String],[(String,Int)],[String])
evalVar (a,b,c) = ((findKey (head c) b):a, b, tail c)

evalPlus :: ([String],[(String,Int)],[String]) -> ([String],[(String,Int)],[String])
evalPlus (a,b,c) = (show(addOp x y):na,b,tail c)
    where   x = read (a !! 0) :: Int
            y = read (a !! 1) :: Int
            na = drop 2 a

evalMinus :: ([String],[(String,Int)],[String]) -> ([String],[(String,Int)],[String])
evalMinus (a,b,c) = (show(subOp x y):na,b,tail c)
    where   x = read (a !! 0) :: Int
            y = read (a !! 1) :: Int
            na = drop 2 a

evalMul :: ([String],[(String,Int)],[String]) -> ([String],[(String,Int)],[String])
evalMul (a,b,c) = (show(mulOp x y):na,b,tail c)
    where   x = read (a !! 0) :: Int
            y = read (a !! 1) :: Int
            na = drop 2 a

-- TODO eval infix expressions
evalExp :: ([String],[(String,Int)],[String]) -> ([String],[(String,Int)],[String])
evalExp (a,b,c)
    | c == [] = (a,b,c)
    -- TODO use any letter as variable!
    | x == "v" = evalExp (evalVar (a,b,c))
    | x == "+" = evalExp (evalPlus (a,b,c))
    | x == "-" = evalExp (evalMinus (a,b,c))
    | x == "*" = evalExp (evalMul (a,b,c))
    | otherwise = evalExp (x:a,b,tail c)
    where x = head c
