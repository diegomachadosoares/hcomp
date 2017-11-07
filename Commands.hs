module Commands where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.List
import Data.Char

import Syntax
import HelperTools
import Expressions
import Declaration

evalNil :: (E, S, M, C) -> (E, S, M, C)
evalNil (e,s,m,c) = (e,s,m,tail c)

evalAttr :: (E, S, M, C) -> (E, S, M, C)
evalAttr (e,s,m,c)
    | x == -1 = (e,s,m,c)
    | otherwise = (e, tail s,(m V.// [(x, (head s))]),tail c)
    where x = read (snd (Map.findWithDefault ("-1","-1") (head c) e))

evalIF :: (E, S, M, C) -> (E, S, M, C)
evalIF (e,s,m,c)
    | x == "tt" = (e,v,y,(tail (dropWhile (/="fimElse") c)))
    | x == "ff" = (e,k,z,tail (dropWhile (/="fimElse") c))
    where x = head (filterS (evalExp(e,s,m,takeWhile (/="then") c)))
          v = filterS (evalGen (e,s,m, (takeWhile (/="else") (tail (dropWhile (/="then") c)))))
          y = filterM (evalGen (e,s,m, (takeWhile (/="else") (tail (dropWhile (/="then") c)))))
          k = filterS (evalGen (e,s,m, (takeWhile (/="fimElse") (tail (dropWhile (/="else") c)))))
          z = filterM (evalGen (e,s,m, (takeWhile (/="fimElse") (tail (dropWhile (/="else") c)))))

evalWhile :: (E, S, M, C) -> (E, S, M, C)
evalWhile (e,s,m,c)
    | x == "tt" = evalCMD(e,v,y,"while":c)
    | x == "ff" = (e,s,m,tail (dropWhile (/="fimDo") c))
    where x = head (filterS (evalExp(e,s,m,takeWhile (/="do") c)))
          v = filterS (evalGen (e,s,m, (takeWhile (/="fimDo") (tail (dropWhile (/="do") c)))))
          y = filterM (evalGen (e,s,m, (takeWhile (/="fimDo") (tail (dropWhile (/="do") c)))))

evalCMD :: (E, S, M, C) -> (E, S, M, C)
evalCMD (e,s,m,c)
    | null c = (e,s,m,c)
    | x == "nil" = evalCMD (evalNil (e,s,m,c))
    | x == ":=" = evalCMD (evalAttr (e,s,m,tail c))
    | x == "if" = evalCMD (evalIF (e,s,m,tail c))
    | x == "while" = evalCMD (evalWhile (e,s,m,tail c))
    | x == "fimElse" = (e,s,m,tail c)
    | x == "var" = evalDec (e,s,m,c)
    | otherwise = (e,s,m,c)
    where x = head c

evalGen :: (E, S, M, C) -> (E, S, M, C)
evalGen (e,s,m,c)
    | null c = (e,s,m,c)
    | x `elem` vars = evalGen (evalExp (e,s,m,c))
    | x == "+" = evalGen (evalExp (e,s,m,c))
    | x == "-" = evalGen (evalExp (e,s,m,c))
    | x == "*" = evalGen (evalExp (e,s,m,c))
    | x == "tt" = evalGen (evalExp (e,s,m,c))
    | x == "ff" = evalGen (evalExp (e,s,m,c))
    | x == "=" = evalGen (evalExp (e,s,m,c))
    | x == "or" = evalGen (evalExp (e,s,m,c))
    | x == "~" = evalGen (evalExp (e,s,m,c))
    | x == "nil" = evalGen (evalCMD (e,s,m,c))
    | x == ":=" = evalGen (evalCMD (e,s,m,c))
    | x == "if" = evalGen (evalCMD (e,s,m,c))
    | x == "while" = evalGen (evalCMD (e,s,m,c))
    | x == "const" = evalGen (evalDec (e,s,m,c))
    | x == "var" = evalGen (evalDec (e,s,m,c))
    | isDigit (head x) = evalGen (e,x:s,m,tail c)
    | otherwise = (e,s,m,c)
    where x = head c