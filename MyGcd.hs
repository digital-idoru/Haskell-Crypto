module MyGcd (myGcd) where

--Compute the gcd using the Euclidean algorithm,
myGcd :: Integral a => a -> a -> a -> a
myGcd x y r = if y <= 0 then x else
	myGcd y (x `mod` y) (x `mod` y) 