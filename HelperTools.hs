module HelperTools where
import Syntax

filterS :: (a,b,c,d) -> (b)
filterS (_,s,_,_) = (s)

filterM :: (a,b,c,d) -> (c)
filterM (_,_,m,_) = (m)

convertLoc :: Loc -> Int
convertLoc (Loc a) = a

convStr :: Str -> Value
convStr (ValueI a) = ValI a
convStr (ValueB b) = ValB b

convValStr :: Value -> Str
convValStr (ValI a) = ValueI a
convValStr (ValB a) = ValueB a

convValBnd :: Value -> Bnd
convValBnd (ValI a) = BndVal (ValueI a)
convValBnd (ValB a) = BndVal (ValueB a)

convBnd :: Bnd -> Value
convBnd (BndVal b) = convStr(b)

isLoc :: Bnd -> Bool
isLoc (BndLoc _) = True
isLoc _ = False

isStr :: Bnd -> Bool
isStr (BndVal _) = True
isStr _ = False

getVar :: Exp -> [Char]
getVar (Evar a) = a

getVal :: Exp -> Value
getVal (EBool a) = ValB a

rBnd :: Bnd -> Int
rBnd (BndLoc (Loc a)) = a

rIVal :: Value -> Int
rIVal (ValI a) = a

rBVal :: Value -> Bool
rBVal (ValB a) = a

isTrue :: Value -> Bool
isTrue (ValB True) = True
isTrue _ = False

isFalse :: Value -> Bool
isFalse (ValB False) = True
isFalse _ = False

