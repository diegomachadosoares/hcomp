module Main where

import qualified Data.Map as Map
import qualified Data.Vector as V

import Expressions
import Syntax
--import Commands
--import ESMC

--a :: [String]
--a = ["10","20","20","30","20","50"]
b = [ValueI 1, ValueI 10, ValueI 100]

m :: V.Vector Str
m = V.fromList b

env1 :: Map.Map String Bnd
env1 = Map.insert "const_A" (BndVal $ ValueI 1) Map.empty
env2 = Map.insert "const_B" (BndVal $ ValueI 0) env1
env3 = Map.insert "var_A" (BndLoc (Loc 0)) env2
env4 = Map.insert "var_B" (BndLoc (Loc 1)) env3
env = Map.insert "var_C" (BndLoc (Loc 2)) env4

-- | Expressions
varExpr = (env,[],m,[Cvar "var_A", Cvar "const_A"])
sumExpr = (env,[],m,[(Cvar "const_A"),(Cvar "var_A"),Cexp Add])
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
    -- | Expressions Tests
    print("Expressions")
    print $ evalExp varExpr
    print $ evalExp sumExpr
    {-
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
   {- print (eval dec)
    print ("---")
    print (eval decIF)
    print ("---")
    print (eval decIFVar)
    print ("---")
    print (eval decIFN)
    print ("---")
    print (eval decIFNVar)
-}