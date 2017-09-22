-- Eval.hs
module Eval where

import Syntax

data Tree a = Empty | Node a (Tree a) (Tree a) deriving(Eq,Ord,Show)

evalExp :: Expr -> Int
evalExp exp = case exp of
  Add x y -> eval x + eval y
  Sub x y -> eval x - eval y
  Mul x y -> eval x * eval y
  Div x y -> eval x / eval y
