module Main where

import qualified Data.Map as Map
import Expressions
import BooleanExpressions
import Commands
import ESMC

m0 = Map.empty
m1 = Map.insert "a" "1" Map.empty
m2 = Map.insert "b" "2" m1
m3 = Map.insert "c" "2" m2
m4 = Map.insert "d" "3" m3
m = Map.insert "e" "2" m4

env = Map.empty

-- Expressions
varExpr = (env,[],m,["a","b"])
sumExpr = (env,[],m,["1","2","+"]) -- 3
sumExpr1 = (env,[],m,["a","b","+"]) -- 3
subExpr = (env,[],m,["1","2","-"]) -- 1
subExpr1 = (env,[],m,["c","d","-"]) -- 1
mulExpr = (env,[],m,["1","2","*"]) -- 2
compExpr = (env,[],m,["1","2","+","6","3","-","*"]) -- 9
compExpr2 = (env,[],m,["1","2","+","6","3","-","*","1","+"]) -- 10

-- BooleanExpressions
trueExpr = (env,[],m,["tt"])
falseExpr = (env,[],m,["ff"])
eqExpr = (env,[],m,["a","b","="])
eqExpr1 = (env,[],m,["c","b","="])
eqExpr2 = (env,[],m,["a","1","="])
eqExpr3 = (env,[],m,["1","2","="])
orExpr = (env,[],m,["tt","tt","or"])
orExpr0 = (env,[],m,["tt","ff","or"])
orExpr1 = (env,[],m,["ff","ff","or"])
negExpr = (env,[],m,["tt","~"])
negExpr1 = (env,[],m,["ff","~"])

-- Commands
nilCmd = (env,[],m,["nil"])
attrCmd = (env,[],m,["2",":=","a","6",":=","b"])
ifCmd = (env,[],m,["if","a","1","=","then","10",":=","c","else","2",":=","c"])
ifCmd1 = (env,[],m,["if","e","6","=","then","1",":=","c","else","8",":=","c"])
whileCmd = (env,[],m,["while","b","0","=","~","do","b","1","-",":=","b","fimDo"])

fact = (env,[],m,["4",":=","x","1",":=","y","while","x","0","=","~","do","x","y","*",":=","y","x","1","-",":=","x","fimDo"])


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

    -- Generic Eval --
    print ("Generic eval")
    print (eval varExpr)
    print (eval sumExpr)
    print (eval trueExpr)
    print (eval eqExpr1)
    print (eval negExpr)
    print (eval attrCmd)
    print (eval ifCmd)
    print (eval whileCmd)
