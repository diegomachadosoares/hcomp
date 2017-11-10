module Main where

import qualified Data.Map as Map
import qualified Data.Vector as V

import Expressions
import Syntax
import Commands
import HelperTools
--import ESMC

b = [ValueI 1, ValueI 10, ValueI 100]

m :: V.Vector Str
m = V.fromList b

env1 :: Map.Map String Bnd
env1 = Map.insert "const_A" (BndVal $ ValueI 1) Map.empty
env2 = Map.insert "const_B" (BndVal $ ValueI 2) env1
env3 = Map.insert "var_A" (BndLoc (Loc 0)) env2
env4 = Map.insert "var_B" (BndLoc (Loc 1)) env3
env = Map.insert "var_C" (BndLoc (Loc 2)) env4
{-
-- | Expressions
varExpr = (env,[],m,[Cvar "var_A", Cvar "const_A"])
sumExpr = (env,[],m,[Cvar "const_A", Cvar "var_A", Cexp Add])
subExpr = (env,[],m,[Cvar "const_A", Cvar "const_B", Cexp Sub])
mulExpr = (env,[],m,[Cvar "var_A", Cvar "var_B", Cexp Mul])
compExpr = (env,[],m,[Cvar "var_A", Cvar "var_B", Cexp Add, Cvar "const_A", Cvar "const_B", Cexp Mul, Cexp Add])
-}
-- | BooleanExpressions
trueExpr = (env,[],m,[Cexp [EBool True]])

-- | Commands
nilCmd = (env,[],m,[Ccom Nill])
ifCmd = (env,[],m,[Ccom (If [EBool True] [(Ccom (Attr "var_A" [Num 100]))] [(Ccom (Attr "var_A" [Num 1000]))])])
ifNil = (env,[],m,[Ccom (If [EBool True] [Ccom Nill] [(Ccom (Attr "var_A" [Num 1000]))])])
whileCmd = (env,[],m,["while","e","0","=","~","do","e","1","-",":=","e","fimDo"])

fact = (env,[],m,[(Ccom (Var "var_D" "int" [Num 3])),
    (Ccom (While [(Evar "var_D"),(Num 0),(Eq)]
    [(Ccom (Attr "var_A" [(Evar "var_A"),(Evar "var_D"),Mul]))
    ,(Ccom (Attr "var_D" [(Evar "var_D"),(Num 1),Sub]))]))])


-- | Declaration
dec = (env, [], m, ["var", "int", "100", ":=", "x"])
decIF = (env,[], m, ["const", "int", "if", "tt", "then", "10", "else", "1", "fimElse", "x"])
decIFVar = (env,[], m, ["var", "int", "if", "tt", "then", "10", "else", "1", "fimElse", "x"])
decIFN = (env,[], m, ["const", "int", "if", "ff", "then", "10", "else", "19", "fimElse", "x"])
decIFNVar = (env,[], m, ["var", "int", "if", "ff", "then", "10", "else", "19", "fimElse", "x"])

main = do
    {-- | Expressions Tests
    print("Expressions")
    print $ evalExp varExpr
    print $ evalExp sumExpr
    print $ evalExp subExpr
    print $ evalExp mulExpr
    print $ evalExp compExpr-}

    {-
    -- | Boolean Expressions Test
    print $ "Boolean Expressions"
    print $ evalExp trueExpr
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

    -- | Commands
    {-
    print ("Commands")
    print $ evalCMD nilCmd
    print (evalCMD attrCmd)
    print (evalCMD whileCmd)
    print (evalCMD ifCmd)
    print (evalCMD ifCmd1)
    print (evalCMD whileCmd)
    -}

    -- | Factorial

    print ("Factorial")
    print (filterM (evalCMD fact))

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