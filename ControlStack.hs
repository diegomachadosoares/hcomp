module ControlStack where

import Syntax

data Tree a = Empty | Node a [(Tree a)] deriving(Eq,Ord,Show)

stackBlock :: Tree String -> [String] -> [String]
stackBlock ((a [(x):(y):xs]) l)
   |a == ";" = StackBlock (y StackBlock(x l))
   |a == "if" = "if":StackBlock (x "then":StackBlock(y "else":StackBlock(head xs l)))
   |a == "while" = "while":StackBlock(x "do":StackBlock(y "fimDo":StackBlock(head xs l)))
   |a == "+" = StackBlock (x StackBlock(y "+":l))
   |a == "-" = StackBlock (x StackBlock(y "-":l))
   |a == "*" = StackBlock (x StackBlock(y "*":l))
   |a == "/" = StackBlock (x StackBlock(y "/":l))
   |a == "=" = StackBlock (x StackBlock(y "=":l))
   |a == "or" = StackBlock (x StackBlock(y "or":l))
   |a == "~" = StackBlock (x StackBlock(y "~":l))
   |a == ":=" = StackBlock (x StackBlock(y ":=":l))
   |otherwise = a:l
   
 funcSMC :: Tree String -> ([String],Map.Map String String,[String])
 funcSMC x = EvalGen ([],[],StackBlock x)
 
 evalGen :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
 evalGen (a,b,c)
   | null c = (a,b,c)
   | x == "v" = evalExp (evalVar (a,b,c))
   | x == "+" = evalExp (evalPlus (a,b,c))
   | x == "-" = evalExp (evalMinus (a,b,c))
   | x == "*" = evalExp (evalMul (a,b,c))
   | x == "nil" = evalCMD (evalNil (a,b,c))
   | x == ":=" = evalCMD (evalAttr (a,b,c))
   | x == "if" = evalCMD (evalIF (a,b,tail c))
   | x == "tt" = evalBoolean (evalT (a,b,c))
   | x == "ff" = evalBoolean (evalT (a,b,c))
   | x == "=" = evalBoolean (evalEq (a,b,c))
   | x == "or" = evalBoolean (evalOr (a,b,c))
   | x == "~" = evalBoolean (evalTil(a,b,c))
   | otherwise = evalGen (x:a,b,tail c)
    where x = head c