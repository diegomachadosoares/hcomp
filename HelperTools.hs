module HelperTools where

import Syntax

filterS :: (a,b,c,d) -> (b)
filterS (_,s,_,_) = (s)

isValue :: EnvVal -> Bool
isValue (ValueI _) = True
isValue (ValueB _) = True
isValue _ = False

isLoc :: EnvVal -> Bool
isLoc (Loc _) = True
isLoc _ = False