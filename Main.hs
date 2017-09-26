module Main where

import qualified Data.Map as Map
import Expressions
import BooleanExpressions
import Commands

m0 = Map.empty
m1 = Map.insert "a" "1" Map.empty
m2 = Map.insert "b" "2" m1
m3 = Map.insert "c" "2" m2
m4 = Map.insert "d" "3" m3
m = Map.insert "e" "2" m4

-- Expressions
varExpr = ([],m,["a","b"])
sumExpr = ([],m,["1","2","+"]) -- 3
sumExpr1 = ([],m,["a","b","+"]) -- 3
subExpr = ([],m,["1","2","-"]) -- 1
subExpr1 = ([],m,["c","d","-"]) -- 1
mulExpr = ([],m,["1","2","*"]) -- 2
compExpr = ([],m,["1","2","+","6","3","-","*"]) -- 9
compExpr2 = ([],m,["1","2","+","6","3","-","*","1","+"]) -- 10

-- BooleanExpressions
trueExpr = ([],m,["tt"])
falseExpr = ([],m,["ff"])
eqExpr = ([],m,["a","b","="])
eqExpr1 = ([],m,["c","b","="])
eqExpr2 = ([],m,["a","1","="])
eqExpr3 = ([],m,["1","2","="])
orExpr = ([],m,["tt","tt","or"])
orExpr0 = ([],m,["tt","ff","or"])
orExpr1 = ([],m,["ff","ff","or"])
negExpr = ([],m,["tt","~"])
negExpr1 = ([],m,["ff","~"])

-- Commands
nilCmd = ([],m,["nil"])
attrCmd = ([],m,["2",":=","a","6",":=","b"])
ifCmd = ([],m,["if","a","1","=","then","10",":=","c","else","2",":=","c"])
ifCmd1 = ([],m,["if","e","6","=","then","1",":=","c","else","8",":=","c"])
whileCmd = ([],m,["while","b","0","=","~","do","b","1","-",":=","b","fimDo"])

fact = ([],m,["4",":=","x","1",":=","y","while","x","0","=","~","do","x","y","*",":=","y","x","1","-",":=","x","fimDo"])


main = do
    -- Expressions Tests
    print("Expressions")
    print (evalExp varExpr)
    print (evalExp sumExpr)
    print (evalExp sumExpr1)
    print (evalExp subExpr)
    print (evalExp subExpr1)
    print (evalExp mulExpr)
    print (evalExp compExpr)
    print (evalExp compExpr2)
    -- Boolean Expressions Test
    print ("Boolean Expressions")
    print (evalBoolean trueExpr)
    print (evalBoolean falseExpr)
    print (evalBoolean eqExpr)
    print (evalBoolean eqExpr1)
    print (evalBoolean eqExpr2)
    print (evalBoolean eqExpr3)
    print (evalBoolean orExpr)
    print (evalBoolean orExpr0)
    print (evalBoolean orExpr1)
    print (evalBoolean negExpr)
    print (evalBoolean negExpr1)
    -- Commands
    print ("Commands")
    print (evalCMD nilCmd)
    print (evalCMD attrCmd)
    print (evalCMD ifCmd)
    print (evalCMD ifCmd1)
    print (evalCMD whileCmd)
    -- Factorial
    print ("Factorial")
    print (evalCMD fact)
