module Main where

import qualified Data.Map as Map
import qualified Data.Vector as V

import Expressions
import Commands
import ESMC

a :: [String]
a = ["10","20","20","30","20","50"]
b = []

m :: V.Vector String
m = V.fromList b

env1 :: Map.Map String (String,String)
env1 = Map.insert "a" ("c","1") Map.empty
env2 = Map.insert "b" ("v","0") env1
env3 = Map.insert "c" ("v","1") env2
env4 = Map.insert "d" ("c","4") env3
env = Map.insert "e" ("v","2") env4

-- | Expressions
varExpr = (env,[],m,["a","b"])
sumExpr = (env,[],m,["1","2","+"]) -- 3
sumExpr1 = (env,[],m,["a","b","+"]) -- 3
subExpr = (env,[],m,["1","2","-"]) -- 1
subExpr1 = (env,[],m,["c","d","-"]) -- 1
mulExpr = (env,[],m,["1","2","*"]) -- 2
compExpr = (env,[],m,["1","2","+","6","3","-","*"]) -- 9
compExpr2 = (env,[],m,["1","2","+","6","3","-","*","1","+"]) -- 10

-- | BooleanExpressions
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

-- | Commands
nilCmd = (env,[],m,["nil"])
attrCmd = (env,[],m,["2",":=","a","6",":=","b"])
ifCmd = (env,[],m,["if","a","1","=","then","10",":=","c","else","2",":=","c","fimElse","200",":=","e"])
ifCmd1 = (env,[],m,["if","e","6","=","then","1",":=","c","else","8",":=","c","fimElse"])
whileCmd = (env,[],m,["while","e","0","=","~","do","e","1","-",":=","e","fimDo"])

fact = (env,[],m,["4",":=","x","1",":=","y","while","x","0","=","~","do","x","y","*",":=","y","x","1","-",":=","x","fimDo"])


-- | Declaration
dec = (env, [], m, ["var", "int", "100", ":=", "x"])
decIF = (env,[], m, ["const", "int", "if", "tt", "then", "10", "else", "1", "fimElse", "x"])
decIFVar = (env,[], m, ["var", "int", "if", "tt", "then", "10", "else", "1", "fimElse", "x"])
decIFN = (env,[], m, ["const", "int", "if", "ff", "then", "10", "else", "19", "fimElse", "x"])
decIFNVar = (env,[], m, ["var", "int", "if", "ff", "then", "10", "else", "19", "fimElse", "x"])

main = do
    {- | Expressions Tests
    print("Expressions")
    print (evalExp varExpr)
    print (evalExp sumExpr)
    print (evalExp sumExpr1)
    print (evalExp subExpr)
    print (evalExp subExpr1)
    print (evalExp mulExpr)
    print (evalExp compExpr)
    print (evalExp compExpr2)
    -}

    {- | Boolean Expressions Test
    print ("Boolean Expressions")
    print (evalExp trueExpr)
    print (evalExp falseExpr)
    print (evalExp eqExpr)
    print (evalExp eqExpr1)
    print (evalExp eqExpr2)
    print (evalExp eqExpr3)
    print (evalExp orExpr)
    print (evalExp orExpr0)
    print (evalExp orExpr1)
    print (evalExp negExpr)
    print (evalExp negExpr1)
    -}

    {- | Commands
    print ("Commands")
    print (evalCMD nilCmd)
    print (evalCMD attrCmd)
    print (evalCMD ifCmd)
    print (evalCMD ifCmd1)
    print (evalCMD whileCmd)
    -}

    {- | Factorial
    print ("Factorial")
    print (evalCMD fact)
    -}

    {- | Generic Eval
    print ("Generic eval")
    print (eval varExpr)
    print (eval sumExpr)
    print (eval trueExpr)
    print (eval eqExpr1)
    print (eval negExpr)
    print (eval attrCmd)
    print (eval ifCmd)
    print (eval whileCmd)
    -}
    -- | print (eval dec)
    -- | print (eval decIF)
    -- | print (eval decIFVar)
    -- | print (eval decIFN)
    print (eval decIFNVar)
