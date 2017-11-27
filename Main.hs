module Main where

import qualified Data.Map as Map
import qualified Data.Vector as V

import Text.Pretty.Simple (pPrint)

import Expressions
import Syntax
import Commands
import HelperTools
import Parser
--import ESMC

m :: V.Vector Str
m = V.fromList [ValueI 5, ValueI 10, ValueI 100]

env1 :: Map.Map String Bnd
env1 = Map.insert "constA" (BndVal $ ValueI 1) Map.empty
env2 = Map.insert "constB" (BndVal $ ValueI 2) env1
env3 = Map.insert "varA" (BndLoc (Loc 0)) env2
env4 = Map.insert "varB" (BndLoc (Loc 1)) env3
env = Map.insert "varC" (BndLoc (Loc 2)) env4

{-
-- | Expressions
varExpr = (env,[],m,[Evar "varA", Evar "constA",Eq,Not])
sumExpr = (env,[],m,[Evar "constA", Evar "varA", Add])
subExpr = (env,[],m,[Evar "constA", Evar "constB", Sub])
mulExpr = (env,[],m,[Evar "varA", Evar "varB", Mul])
compExpr = (env,[],m,[Evar "varA", Evar "varB", Add, Evar "constA", Evar "constB", Mul, Add])

-}
-- | BooleanExpressions
trueExpr = (env,[],m,[Cexp $ Not (EBool True)])
falseExpr = (env,[],m,[Cexp $ Not (EBool False)])
eqExpr = (env,[],m,[Cexp $ Not (Eq (Num 0) (Num 0))])

{-
-- | Commands

nilCmd = (env,[],m,[Ccom Nill])
ifCmd = (env,[],m,[Ccom (If [EBool True] [(Ccom (Attr "varA" [Num 100]))] [(Ccom (Attr "varA" [Num 1000]))])])
ifNil = (env,[],m,[Ccom (If [EBool True] [Ccom Nill] [(Ccom (Attr "varA" [Num 1000]))])])
whileCmd = (env,[],m,[(Ccom (Var "varD" "int" [Num 10]))
                     ,(Ccom (While [(Evar "varD"),(Num 0),(Eq),Not]
                     [(Ccom (Attr "varD" [(Evar "varD"),(Num 1),Sub]))]))])
-}
-- | Factorial
fact = (env,[],m,[(Ccom (Var "varD" "int" (Num 3))),
    (Ccom (While (Not (Eq (Evar "varD") (Num 0)))
    [(Ccom (Attr "varA" (Mul (Evar "varA") (Evar "varD"))))
    ,(Ccom (Attr "varD" (Sub (Evar "varD") (Num 1))))]))])

-- | Parser
parserIf = (env,[],m,[Ccom (parseString "if varA = 5 then varA := 10 else varA := 2")])
parserWhile = (env,[],m,[Ccom (parseString "while (not (varA = 10)) do varA := varA + 1")])
parser = (env,[],m,[Ccom (parseString "{ if 2 = 2 then varA := 1 else varA := 2 end; { varA := 15 ; varA := 7 } }")],[])

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
    pPrint $ evalExp falseExpr
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

    {-
    -- | Factorial
    pPrint ("Factorial")
    pPrint $ evalCMD (fact)
    -}

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

    -- | Parser
    --pPrint $ evalCMD (parserIf)
    --pPrint $ evalCMD (parserWhile)
    pPrint $ evalCMD (parser)