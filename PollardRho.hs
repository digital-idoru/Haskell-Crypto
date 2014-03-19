--Daniel Hono
-- 2014 

module PollardRho (rho) where


-- The Pollard-Rho is an approximation algorithm, i.e. it is not gaurenteed to return a correct value everytime. 
-- The case where it doesn't work, we just return Nothing. 


import Data.Maybe
import Russian_Peasant
import MyGcd

-- f and g below are used to compute psuedo-random sequences of numbers mod n, to check for factors. 
f :: Integral a => a -> a -> a
f x n = ((rP 2 x n) + 1) `mod` n

g :: Integral a => a -> a -> a
g x n = ((rP 4 x n) + (2 * (rP 2 x n)) + 2) `mod` n

-- Use the Rho method with Floyd's cycle detection. 
-- Approximation algorithm, so we handle the case where it could fail. If function returns nothing, then we need to try again with a different seed or 
-- psuedo-random sequence generator. 
-- Expected running time O(sqrt(p)) by the Birthday Problem 
rho :: Integral a => a -> a -> a -> Maybe a
rho n x y 
	| k == n = Nothing -- Test has failed
	| k == 1 = rho n (f x n) (g y n) 
	| otherwise = Just k 
		where k = (myGcd (abs ((f x n) - (g y n))) n 1 )



