module ControlStack where

import Syntax

data Tree a = Empty | Node a (Tree a) (Tree a) (Tree a) deriving(Eq,Ord,Show)

stackBlock :: Tree String -> [String] -> [String]
let stackBlock (Node a (Tree x) (Tree y) (Tree z)) l
  |a == ";" = StackBlock (y StackBlock(x l))
  |a == "if" = "if":StackBlock (x "then":StackBlock(y "else":StackBlock(z l)))
  |a == "while" = "while":StackBlock(x "do":StackBlock(y "fimDo":StackBlock(z l)))
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
funcSMC x = EvalGen ([],[],StackBlock x [])
 
evalGen :: ([String],Map.Map String String,[String]) -> ([String],Map.Map String String,[String])
let evalGen (a,b,c)
   | null c = (a,b,c)
   | x == "v" = evalGen evalExp (a,b,c)
   | x == "+" = evalGen evalExp (a,b,c)
   | x == "-" = evalExp (a,b,c)
   | x == "*" = evalExp (a,b,c)
   | x == "nil" = evalGen evalCMD (a,b,c)
   | x == ":=" = evalGen evalCMD (a,b,c)
   | x == "if" = evalGen evalCMD (a,b,tail c)
   | x == "tt" = evalGen evalBoolean (a,b,c)
   | x == "ff" = evalGen evalBoolean (a,b,c)
   | x == "=" = evalGen evalBoolean (a,b,c)
   | x == "or" = evalGen evalBoolean (a,b,c)
   | x == "~" = evalGen evalBoolean (a,b,c))
   | otherwise = evalGen evalGen (x:a,b,tail c)
    where x = head c