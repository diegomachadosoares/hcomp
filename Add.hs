-- Add.hs
module Add (add) where

import Data.Char

data Expression =
  Num Int
  | Add Expression Expression
  | Sub Expression Expression
  deriving (Show)

expected x = error $ x ++ " expected"

term x
  | isDigit x = Num x
  | otherwise = expected "Digit"

addOperation x
  | x == '+' = Add
  | x == '-' = Sub
  | otherwise = expected "AddOp"

add :: Integer -> Integer -> Integer
add x y = x + y

sub :: Integer -> Integer -> Integer
sub x y = x - y

expression (x:[]) = term x
expression (x:y:zs) = (addOperation y) (expression [x]) (expression zs)
