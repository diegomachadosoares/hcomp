module Main where

import qualified Data.Map as Map
import Expressions

m = Map.insert "v" "1" Map.empty

varExpr = ([],m,["v"])
sumExpr = ([],m,["1","2","+"]) -- 3
subExpr = ([],m,["1","2","-"]) -- 1
mulExpr = ([],m,["1","2","*"]) -- 2
compExpr = ([],m,["1","2","+","6","3","-","*"]) -- 9
compExpr2 = ([],m,["1","2","+","6","3","-","*","1","+"]) -- 10

main = do
    print (evalExp varExpr)
    print (evalExp sumExpr)
    print (evalExp subExpr)
    print (evalExp mulExpr)
    print (evalExp compExpr)
    print (evalExp compExpr2)
