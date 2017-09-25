module Main where

import qualified Data.Map as Map
import Expressions
import BooleanExpressions
import Commands

m = Map.empty
m1 = Map.insert "v" "1" Map.empty

-- Expressions
varExpr = ([],m1,["v"])
sumExpr = ([],m,["1","2","+"]) -- 3
subExpr = ([],m,["1","2","-"]) -- 1
mulExpr = ([],m,["1","2","*"]) -- 2
compExpr = ([],m,["1","2","+","6","3","-","*"]) -- 9
compExpr2 = ([],m,["1","2","+","6","3","-","*","1","+"]) -- 10

-- BooleanExpressions
trueExpr = ([],m,["tt"])
falseExpr = ([],m,["ff"])
eqExpr = ([],m,["1","1","="])
eqExpr1 = ([],m,["1","2","="])
orExpr = ([],m,["tt","tt","or"])
orExpr0 = ([],m,["tt","ff","or"])
orExpr1 = ([],m,["ff","ff","or"])
negExpr = ([],m,["tt","~"])
negExpr1 = ([],m,["ff","~"])

-- Commands
nilCmd = ([],m,["nil"])
attrCmd = ([],m,["v","2",":=", "a","1",":=","a","2",":="])
ifCmd = ([],m,["if","tt","then","c","1",":=","else","c","2",":="])
ifCmd1 = ([],m,["if","ff","then","c","1",":=","else","c","2",":="])


main = do
    -- Expressions Tests
    print("Expressions")
    print (evalExp varExpr)
    print (evalExp sumExpr)
    print (evalExp subExpr)
    print (evalExp mulExpr)
    print (evalExp compExpr)
    print (evalExp compExpr2)
    -- Boolean Expressions Test
    print ("Boolean Expressions")
    print (evalBoolean trueExpr)
    print (evalBoolean falseExpr)
    print (evalBoolean eqExpr)
    print (evalBoolean eqExpr1)
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
