module Main where

import qualified Data.Map as Map
import qualified Data.Vector as V

import Expressions
import Syntax
import Commands
import HelperTools
--import ESMC



m :: V.Vector Str
m = V.fromList [ValueI 1, ValueI 10, ValueI 100]

env1 :: Map.Map String Bnd
env1 = Map.insert "const_A" (BndVal $ ValueI 1) Map.empty
env2 = Map.insert "const_B" (BndVal $ ValueI 2) env1
env3 = Map.insert "var_A" (BndLoc (Loc 0)) env2
env4 = Map.insert "var_B" (BndLoc (Loc 1)) env3
env = Map.insert "var_C" (BndLoc (Loc 2)) env4


-- | Expressions
varExpr = (env,[],m,[Evar "var_A", Evar "const_A",Eq,Not])
sumExpr = (env,[],m,[Evar "const_A", Evar "var_A", Add])
subExpr = (env,[],m,[Evar "const_A", Evar "const_B", Sub])
mulExpr = (env,[],m,[Evar "var_A", Evar "var_B", Mul])
compExpr = (env,[],m,[Evar "var_A", Evar "var_B", Add, Evar "const_A", Evar "const_B", Mul, Add])

-- | BooleanExpressions
trueExpr = (env,[],m,[EBool True])
falseExpr = (env,[],m,[EBool False])

-- | Commands
nilCmd = (env,[],m,[Ccom Nill])
ifCmd = (env,[],m,[Ccom (If [EBool True] [(Ccom (Attr "var_A" [Num 100]))] [(Ccom (Attr "var_A" [Num 1000]))])])
ifNil = (env,[],m,[Ccom (If [EBool True] [Ccom Nill] [(Ccom (Attr "var_A" [Num 1000]))])])
whileCmd = (env,[],m,[(Ccom (Var "var_D" "int" [Num 10]))
                     ,(Ccom (While [(Evar "var_D"),(Num 0),(Eq),Not]
                     [(Ccom (Attr "var_D" [(Evar "var_D"),(Num 1),Sub]))]))])

-- | Factorial
fact = (env,[],m,[(Ccom (Var "var_D" "int" [Num 3])),
    (Ccom (While [(Evar "var_D"),(Num 0),(Eq),Not]
    [(Ccom (Attr "var_A" [(Evar "var_A"),(Evar "var_D"),Mul]))
    ,(Ccom (Attr "var_D" [(Evar "var_D"),(Num 1),Sub]))]))])

main = do
    {-
    -- | Expressions Tests
    print("Expressions")
    print $ filterS $ evalExp varExpr
    print $ filterS $ evalExp sumExpr
    print $ filterS $ evalExp subExpr
    print $ filterS $ evalExp mulExpr
    print $ filterS $ evalExp compExpr
    -}

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

    {-
    -- | Commands
    print ("Commands")
    print $ evalCMD nilCmd
    print (evalCMD attrCmd)
    print (evalCMD ifCmd)
    print (evalCMD whileCmd)
    print (evalCMD ifCmd1)
    print (evalCMD whileCmd)
    -}

    -- | Factorial
    print ("Factorial")
    print $ filterE (evalCMD fact)
    print $ filterS (evalCMD fact)
    print $ filterM (evalCMD fact)

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