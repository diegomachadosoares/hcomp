module Commands where

import qualified Data.Map as Map
import Data.List
import Data.Char

import Syntax
import HelperTools

evalCMD :: (E, S, M, C, O) -> (E, S, M, C, O)
evalCMD (e,s,m,[],o) = (e,s,m,[],o)
evalCMD (e,s,m,(Ccom Nill):c,o) = evalCMD (e,s,m,c,o)
evalCMD (e,s,m,(Ccom (If exp a b)):c,o)
    | x = evalCMD (e,filterS (evalCMD (e,s,m,a,o)),filterM (evalCMD (e,s,m,a,o)),c,o)
    | otherwise = evalCMD (e,filterS (evalCMD (e,s,m,b,o)),filterM (evalCMD (e,s,m,b,o)),c,o)
    where x = rBVal(head (filterS (evalExp(e,s,m,[(Cexp exp)],o))))

evalCMD (e,s,m,(Ccom (While exp a)):c,o)
    | x  = evalCMD(e,filterS (evalCMD(e,s,m,a,o)),filterM (evalCMD(e,s,m,a,o)),(Ccom (While exp a)):c,o)
    | otherwise = evalCMD(e,s,m,c,o)
    where x = rBVal (head (filterS (evalExp(e,s,m,[(Cexp exp)],o))))

evalCMD (e,s,m,(Ccom (Attr a exp)):c,o)
    | x == (Loc (-1)) = (e,s,m,c,o)
    | otherwise = evalCMD (e,s,(Map.insert (x) (convValStr(head (filterS (evalExp (e,s,m,[(Cexp exp)],o)))))  m),c,o)
    where x = rBnd (Map.findWithDefault (BndLoc $ Loc (-1)) a e)

evalCMD (e,s,m,(Ccom (Var a b exp)):c,o) = evalCMD (evalDec(e,s,m,(Ccom (Var a b exp)):c,o))
evalCMD (e,s,m,(Ccom (Const a b exp)):c,o) = evalCMD (evalDec(e,s,m,(Ccom (Const a b exp)):c,o))
evalCMD (e,s,m,(Ccom (Sequence a b)):c,o) = evalCMD (e,s,m,(Ccom a):(Ccom b):c,o)
evalCMD (e,s,m,(Ccom (Print exp)):c,o) = evalCMD (e,s,m,c,head(filterS (evalExp(e,s,m,[(Cexp exp)],o))):o)
evalCMD (e,s,m,(Ccom (Exit a)):c,o) = (Map.empty,[],Map.empty,[(Ccom (Exit a))],o)
evalCMD (e,s,m,(Ccom (ProcA a b)):c,o) = evalCall (e,s,m,(Ccom(ProcA a b)):c,o)
evalCMD (e,s,m,(Ccom (ProcR a f b)):c,o) = evalDec (e,s,m,(Ccom(ProcR a f b)):c,o)
evalCMD (e,s,m,(Ccom (Func a f exp)):c,o) = evalDec (e,s,m,(Ccom(Func a f exp)):c,o)
evalCMD (e,s,m,(Cexp a):c,o) = evalExp (e,s,m,(Cexp a):c,o)
evalCMD (e,s,m,c,o) = (e,s,m,c,o)

-- | Expreções declarativas
evalDecExp :: (E, S, M, C, O) -> (E, S, M, C, O)
evalDecExp (e,s,m,[],o) = (e,s,m,[],o)
evalDecExp (e,s,m,(Cexp (IfExp a b d)):c,o)= evalExpIF (e,s,m,(Cexp (IfExp a b d)):c,o)
evalDecExp (e,s,m,c,o)= evalExp(e,s,m,c,o)

evalExpIF :: (E, S, M, C, O) -> (E, S, M, C, O)
evalExpIF (e,s,m,(Cexp (IfExp a b d)):c,o)
    | x = (e,v,m,tail c,o)
    | otherwise = evalDecExp(e,s,m,tail c,o)
    where x = rBVal (head (filterS (evalExp(e,s,m,[Cexp a],o))))
          v = filterS (evalExp(e,s,m,[Cexp b],o))

-- | Declarações
evalDec :: (E, S, M, C, O) -> (E, S, M, C, O)
evalDec (e,s,m,[],o) = (e,s,m,[],o)
evalDec (e,s,m,(Ccom (Var a b d)):c,o) = free (evalCMD (((Map.insert a (BndLoc (Loc ( pos m)))) e),s,Map.insert (Loc (pos m)) (convValStr(head(filterS (evalDecExp(e,s,m,[Cexp d],o))))) m,c,o)) (pos m)
evalDec (e,s,m,(Ccom (Const a b d)):c,o) = (Map.insert a (convValBnd(head (filterS (evalDecExp(e,s,m,[Cexp d],o))))) e,s,m,c,o)
evalDec (e,s,m,(Ccom (ProcR id f bl)):c,o) = (Map.insert id (BndAbs (f,(Ccom (ProcR id f bl)):bl)) e,s,m,c,o)
evalDec (e,s,m,(Ccom (Func id f exp)):c,o) = (Map.insert id (BndAbsF (f,exp)) e,s,m,c,o)
evalDec (e,s,m,c,o) = (e,s,m,c,o)

-- | Desalocação de memoria
free :: (E, S, M, C, O) -> Int -> (E, S, M, C, O)
free (e,s,m,c,o) i = (e,s,Map.delete (Loc i) m,c,o)

-- | Eval Genérico
evalProg :: (E, S, M, C, O) -> (E, S, M, C, O)
evalProg (e,s,m,[],o) = (e,s,m,[],o)
evalProg (e,s,m,(Ccom (Exit a)):c,o) = (Map.empty,[],Map.empty,[],(ValI (fromIntegral a)):o)
evalProg (e,s,m,c,o) = evalProg(evalCMD(e,s,m,c,o))

-- | Chamada de procedimentos
evalCall :: (E, S, M, C, O) -> (E, S, M, C, O)
evalCall (e,s,m,(Ccom(ProcA a exps)):c,o) = evalCall (e,s,m,(rev1 (ctr exps))++((CALL a):c),o)
evalCall (e,s,m,(Cexp a):c,o) = evalCall ( evalExp (e,s,m,(Cexp a):c,o) )
evalCall (e,s,m,(CALL a):c,o) = (e,(snd (snd (add ((fst (rAbs (Map.findWithDefault (BndAbs ([],[])) a e))) ,([],s))))),m,(fst (snd (add ((fst (rAbs (Map.findWithDefault (BndAbs ([],[])) a e))) ,([],s)))))++(snd (rAbs (Map.findWithDefault (BndAbs ([],[])) a e)))++c,o)

addOp :: Int -> Int -> Int
addOp x y = x + y

subOp :: Int -> Int -> Int
subOp x y = x - y

mulOp :: Int -> Int -> Int
mulOp x y = x * y

evalVar :: (E, S, M, C, O) -> (E, S, M, C, O)
evalVar (e,s,m,c,o)
   | isStr x = (e, (convBnd (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar(head c)) e)):s,m,tail c,o)
   | isLoc x = (e, (convStr  ( Map.findWithDefault (ValueB False) (rBnd (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar $ head c) e)) m)):s, m, tail c,o)
    where x = (Map.findWithDefault (BndLoc $ Loc (-1)) (getVar (head c)) e)


evalPlus :: (E, S, M, C, O) -> (E, S, M, C, O)
evalPlus (e,s,m,c,o) = (e,ValI (addOp x y):ns,m,c,o)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalMinus :: (E, S, M, C, O) -> (E, S, M, C, O)
evalMinus (e,s,m,c,o) = (e,ValI (subOp x y):ns,m,c,o)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalMul :: (E, S, M, C, O) -> (E, S, M, C, O)
evalMul (e,s,m,c,o) = (e,ValI (mulOp x y):ns,m,c,o)
    where   x = rIVal (s !! 1)
            y = rIVal (s !! 0)
            ns = drop 2 s

evalT :: (E, S, M, C, O) -> (E, S, M, C, O)
evalT (e,s,m,c,o) = (e,(getVal (head c)):s,m,tail c,o)

evalEq :: (E, S, M, C, O) -> (E, S, M, C, O)
evalEq (e,s,m,c,o) = if (s !! 0) == (s !! 1) then (e,ValB True:drop 2 s,m,c,o) else (e,ValB False:drop 2 s,m,c,o)

evalOr :: (E, S, M, C, O) -> (E, S, M, C, O)
evalOr (e,s,m,c,o) = if (not(rBVal(s !! 0))  && not(rBVal(s !! 1 ))) then (e,ValB False:drop 2 s,m,c,o) else (e,ValB True:drop 2 s,m,c,o)

evalNot :: (E, S, M, C, O) -> (E, S, M, C, O)
evalNot (e,s,m,c,o)
    | rBVal(head s) = (e,(ValB False):(tail s),m,c,o)
    | otherwise = (e,(ValB True):(tail s),m,c,o)

evalExp :: (E, S, M, C, O) -> (E, S, M, C, O)
evalExp (e,s,m,[],o) = (e,s,m,[],o)
evalExp (e,s,m,(ADD):c,o)= evalExp $ evalPlus (e,s,m,c,o)
evalExp (e,s,m,(SUB):c,o)= evalExp $ evalMinus (e,s,m,c,o)
evalExp (e,s,m,(MUL):c,o)= evalExp $ evalMul (e,s,m,c,o)
evalExp (e,s,m,(EQU):c,o)= evalExp $ evalEq (e,s,m,c,o)
evalExp (e,s,m,(OR):c,o)= evalExp $ evalOr (e,s,m,c,o)
evalExp (e,s,m,(NOT):c,o)= evalExp $ evalNot (e,s,m,c,o)
evalExp (e,s,m,(Cexp (Evar a)):c,o)= evalExp  (evalVar (e,s,m,(Cexp (Evar a)):c,o))
evalExp (e,s,m,(Cexp (Add a b)):c,o) = evalExp  (e,s,m,(Cexp a):(Cexp b):(ADD):c,o)
evalExp (e,s,m,(Cexp (Sub a b)):c,o) = evalExp (e,s,m,(Cexp a):(Cexp b):(SUB):c,o)
evalExp (e,s,m,(Cexp (Mul a b)):c,o) = evalExp (e,s,m,(Cexp a):(Cexp b):(MUL):c,o)
evalExp (e,s,m,(Cexp (EBool a)):c,o) = evalExp (evalT (e,s,m,(Cexp (EBool a)):c,o))
evalExp (e,s,m,(Cexp (Eq a b)):c,o) = evalExp (e,s,m,(Cexp a):(Cexp b):(EQU):c,o)
evalExp (e,s,m,(Cexp (Or a b)):c,o) = evalExp (e,s,m,(Cexp a):(Cexp b):(OR):c,o)
evalExp (e,s,m,(Cexp (Not a)):c,o) = evalExp (e,s,m,(Cexp a):(NOT):c,o)
evalExp (e,s,m,(Cexp (Num a)):c,o) = evalExp (e,(ValI $ fromIntegral a):s,m,c,o)
evalExp (e,s,m,(Cexp (FunA a b)):c,o) = evalFun (e,s,m,(Cexp(FunA a b)):c,o)
evalExp (e,s,m,c,o) = (e,s,m,c,o)

-- | Chamada de Funções
evalFun :: (E, S, M, C, O) -> (E, S, M, C, O)
evalFun (e,s,m,(Cexp (FunA a exps)):c,o) = evalFun (e,s,m,(rev1 (ctr exps))++((CALL a):c),o)
evalFun (e,s,m,(Cexp a):c,o) = evalFun ( evalExp (e,s,m,(Cexp a):c,o) )
evalFun (e,s,m,(CALL a):c,o) = evalCMD (e,(snd (snd (add ((fst (rAbsF (Map.findWithDefault (BndAbsF ([],Num 0)) a e))) ,([],s))))),m,(fst (snd (add ((fst (rAbsF (Map.findWithDefault (BndAbsF ([],Num 0)) a e))) ,([],s)))))++((Cexp (snd (rAbsF (Map.findWithDefault (BndAbsF ([],Num 0)) a e))):c)),o)

-- | Declara as variaveis formais e consome os valores
add :: (F,([Contr],S)) -> (F,([Contr],S))
add ([],(c,s)) = ([],(c,s))
add (a:ids,(c,s)) = add (ids,((Ccom (Var a "int" (Num (fromIntegral(rIVal (head s)))))):c,tail s))
