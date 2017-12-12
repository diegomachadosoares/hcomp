module Main where

import qualified Data.Map as Map
import qualified Data.Vector as V

--import Text.Pretty.Simple (pPrint)

import Syntax
import Commands
import HelperTools
import Parser

m :: V.Vector Str
m = V.fromList [ValueI 5, ValueI 10, ValueI 100]
--empty_M = V.fromList []
empty_M :: Map.Map Loc Str
empty_M = Map.empty

env1 :: Map.Map String Bnd
env1 = Map.insert "constA" (BndVal $ ValueI 1) Map.empty
env2 = Map.insert "constB" (BndVal $ ValueI 2) env1
env3 = Map.insert "varA" (BndLoc (Loc 0)) env2
env4 = Map.insert "varB" (BndLoc (Loc 1)) env3
env = Map.insert "varC" (BndLoc (Loc 2)) env4
empty_Env:: Map.Map String Bnd
empty_Env = Map.empty

-- | Factorial
fact = (env,[],m,[(Ccom (Var "varD" "int" (Num 3)))
    , (Ccom (Attr "varA" (Num 1)))
    ,(Ccom (While (Not (Eq (Evar "varD") (Num 0)))
    [(Ccom (Attr "varA" (Mul (Evar "varA") (Evar "varD"))))
    ,(Ccom (Attr "varD" (Sub (Evar "varD") (Num 1))))]))]
    ,[])

proc =  (env
        ,[]
        ,m
        ,[(Ccom (ProcR "soma" (["a","b"]) [(Ccom (Attr "a" (Add (Evar "a") (Evar "b"))))]))
        ,(Ccom (ProcA "soma" [(Num 1),(Num 2)]))]
        ,[])

func =  (empty_Env
        ,[]
        ,empty_M
        ,[(Ccom (Func "soma" (["a","b"]) (Add (Evar "a") (Evar "b"))))
        ,(Cexp (FunA "soma" [(Num 1),(Num 2)]))]
        ,[])

-- | Parser
parserProc =  (empty_Env
        ,[]
        ,empty_M
        ,[Ccom (parseString " { proc soma ( a int , b int , ) begin { a := a + b } ; { print ( a ) } end } ; { call soma ( 1 , 2 , ) }")]

        ,[])
parserFunc =  (empty_Env
        ,[]
        ,empty_M
        ,[Ccom (parseString " { var a int := 1 } ; { { { func soma ( a int , b int , ) begin a + b end } ; { a := callf soma ( 1 , 2 , ) } } ; { print ( a ) } }")]
        -- PRINT ,[Ccom (parseString " { var a int := 1 } ; { print ( a ) }")]
        ,[])

parserExit =  (empty_Env
        ,[]
        ,empty_M
        ,[Ccom (parseString " { proc soma ( a int , b int , ) begin { a := a + b } ; { { exit 0 } ; { print ( a )} } end } ; { call soma ( 1 , 2 , ) }")]

        ,[])

parserFat =  (empty_Env
        ,[]
        ,empty_M
        ,[Ccom (parseString " { proc fat ( acc int , resp int , ) begin if ( acc = 0)  then  print ( resp ) else { resp := resp * acc }  ; { { acc := acc - 1 } ; { call fat ( acc , resp , ) } } endIF end } ; { call fat ( 3 , 1 , ) } ")]

        ,[])



main = do
    print "$ ---- Evaluating Procedures ---- $"
    print $  filterS (evalProg (parserProc))
    print $ filterM (evalProg (parserProc))
    print $ filterO (evalProg (parserProc))
    print "$ ---- Evaluating Functions ---- $"
    print $ evalProg (parserFunc)
    print "$ ---- Evaluating Exit ---- $"
    print $ evalProg (parserExit)
    --print "$ ---- Evaluating Recursive Procedures ---- $"
    --print $ filterM (evalProg (parserFat))