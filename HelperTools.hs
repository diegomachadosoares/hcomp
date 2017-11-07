module HelperTools where

filterS :: (a,b,c,d) -> (b)
filterS (_,s,_,_) = (s)

filterM :: (a,b,c,d) -> (c)
filterM (_,_,m,_) = (m)