-- From "Modules over Monads and Initial Semantics"
-- Andre Hirschowitz and Marco Maggesi
-- http://web.math.unifi.it/users/maggesi/ic-final/ic.pdf

module SLC where

import Control.Monad (liftM)

data SLC a = Var a | App (SLC a) (SLC a) | Abs (SLC (Maybe a))

{-- 

app : SLC x SLC -> SLC
abs : SLC' -> SLC

--}

instance Monad SLC where
	return = Var
	Var x >>= f = f x
	App x y >>= f = App (x >>= f) (y >>= f)
	Abs x >>= f = Abs (mbind x f)

{-- 
 -
bind f (var v) = f v
bind f (app x y ) = app (bind f x) (bind f y)
bind f (abs x) = abs (mbind f x)

bind (var x) = x

bind g (bind f x) = bind (bind g . f) x

--}
	
mbind :: SLC (Maybe a) -> (a -> SLC b) -> SLC (Maybe b)
mbind x f = x >>= maybe (Var Nothing) (liftM Just . f) 
