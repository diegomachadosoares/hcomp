module Main where

import qualified Data.Map as Map
import qualified Data.Vector as V

import Text.Pretty.Simple (pPrint)

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
    pPrint("Expressions")
    pPrint $ filterS $ evalExp varExpr
    pPrint $ filterS $ evalExp sumExpr
    pPrint $ filterS $ evalExp subExpr
    pPrint $ filterS $ evalExp mulExpr
    pPrint $ filterS $ evalExp compExpr
    -}

    {-
    -- | Boolean Expressions Test
    pPrint $ "Boolean Expressions"
    pPrint $ evalExp trueExpr
    pPrint (evalExp falseExpr)
    pPrint (evalExp eqExpr)
    pPrint (evalExp eqExpr1)
    pPrint (evalExp eqExpr2)
    pPrint (evalExp eqExpr3)
    pPrint (evalExp orExpr)
    pPrint (evalExp orExpr0)
    pPrint (evalExp orExpr1)
    pPrint (evalExp negExpr)
    pPrint (evalExp negExpr1)
    -}

    {-
    -- | Commands
    pPrint ("Commands")
    pPrint $ evalCMD nilCmd
    pPrint (evalCMD attrCmd)
    pPrint (evalCMD ifCmd)
    pPrint (evalCMD whileCmd)
    pPrint (evalCMD ifCmd1)
    pPrint (evalCMD whileCmd)
    -}

    -- | Factorial
    pPrint ("Factorial")
    pPrint $ evalCMD (fact)

    {- | Generic Eval
    pPrint ("Generic eval")
    pPrint (eval varExpr)
    pPrint (eval sumExpr)
    pPrint (eval trueExpr)
    pPrint (eval eqExpr1)
    pPrint (eval negExpr)
    pPrint (eval attrCmd)
    pPrint (eval ifCmd)
    pPrint (eval whileCmd)
    -}
   {- pPrint (eval dec)
    pPrint ("---")
    pPrint (eval decIF)
    pPrint ("---")
    pPrint (eval decIFVar)
    pPrint ("---")
    pPrint (eval decIFN)
    pPrint ("---")
    pPrint (eval decIFNVar)
-}