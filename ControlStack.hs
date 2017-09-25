module ControlStack where

import Syntax

data Tree a = Empty | Node a [(Tree a)] deriving(Eq,Ord,Show)

StackBlock :: Tree String -> [String] -> [String]
StackBlock ((a [(x):(y):xs]) l)
   |a == ";" = StackBlock (y StackBlock(x l))
   |a == "if" = "if":StackBlock (x "then":StackBlock(y "else":StackBlock(head xs l)))
   |a == "while" = "while":StackBlock(x "do":StackBlock(y "fimDo"StackBlock(head xs l)))